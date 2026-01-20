import slicer
import vtk
import numpy as np

# === Adaptive minimum spacing based on mesh diagonal ===
def estimate_min_spacing(polyData, base_spacing=0.3):
    bounds = np.array(polyData.GetBounds()).reshape(3, 2)
    diag = np.linalg.norm(bounds[:, 1] - bounds[:, 0])
    return base_spacing * (diag / 50)

# === Remove points that are too close together (Poisson-disc style) ===
def remove_close_points(points, min_dist=0.3, max_points=100):
    filtered = []
    for pt in points:
        if all(np.linalg.norm(pt - f) >= min_dist for f in filtered):
            filtered.append(pt)
        if len(filtered) >= max_points:
            break
    return np.array(filtered)

# === Main curvature-weighted pseudolandmark generator ===
def generate_curvature_weighted_semilandmarks(modelNode, num_landmarks=100, base_spacing=0.3):
    polyData = modelNode.GetPolyData()
    model_name = modelNode.GetName()

    print(f"🔹 Processing {model_name}...")

    # Step 1: Sample surface points
    sampler = vtk.vtkPolyDataPointSampler()
    sampler.SetInputData(polyData)
    sampler.SetDistance(0.2)  # finer sampling to ensure good candidate pool
    sampler.Update()
    sampledPolyData = sampler.GetOutput()
    n_pts = sampledPolyData.GetNumberOfPoints()

    if n_pts < num_landmarks * 2:
        raise RuntimeError(f"⚠️ Not enough sampled points ({n_pts}) on {model_name}")

    # Step 2: Compute Gaussian curvature
    curvatureFilter = vtk.vtkCurvatures()
    curvatureFilter.SetInputData(polyData)
    curvatureFilter.SetCurvatureTypeToGaussian()  # or use SetCurvatureTypeToMean()
    curvatureFilter.Update()
    curvatureMesh = curvatureFilter.GetOutput()
    curvatureArray = curvatureMesh.GetPointData().GetScalars()
    if curvatureArray is None:
        raise RuntimeError(f"⚠️ Curvature failed for {model_name}")

    # Step 3: Map curvature to sampled points
    locator = vtk.vtkPointLocator()
    locator.SetDataSet(curvatureMesh)
    locator.BuildLocator()

    curvature_vals = np.array([
        curvatureArray.GetTuple1(locator.FindClosestPoint(sampledPolyData.GetPoint(i)))
        for i in range(n_pts)
    ])

    # Step 4: Normalize curvature (z-score)
    mean_curv = np.mean(curvature_vals)
    std_curv = np.std(curvature_vals)
    if std_curv == 0:
        raise RuntimeError(f"⚠️ Zero curvature variance on {model_name}")
    norm_curv = (curvature_vals - mean_curv) / std_curv
    norm_curv = np.abs(norm_curv) + 1e-5  # take abs to emphasize curvature magnitude

    # Step 5: Blend curvature-based and uniform weights
    alpha = 0.8  # prioritize curvature
    uniform_weights = np.ones_like(norm_curv) / len(norm_curv)
    curvature_weights = norm_curv / np.sum(norm_curv)
    blended_weights = alpha * curvature_weights + (1 - alpha) * uniform_weights
    blended_weights /= np.sum(blended_weights)

    # Step 6: Sample points based on weights
    oversample_factor = 4
    sample_count = num_landmarks * oversample_factor
    all_points = np.array([sampledPolyData.GetPoint(i) for i in range(n_pts)])
    sampled_indices = np.random.choice(n_pts, size=min(sample_count, n_pts), replace=False, p=blended_weights)
    candidate_points = all_points[sampled_indices]

    # Step 7: Apply spacing filter
    adaptive_spacing = estimate_min_spacing(polyData, base_spacing)
    selected_points = remove_close_points(candidate_points, min_dist=adaptive_spacing, max_points=num_landmarks)

    # Step 8: Report and place landmarks
    print(f"ℹ️ Target: {num_landmarks}, Retained: {len(selected_points)}, Spacing: {adaptive_spacing:.3f}")

    if len(selected_points) < num_landmarks:
        print(f"⚠️ Only {len(selected_points)} semilandmarks placed on {model_name} due to spacing.")

    fidNode = slicer.mrmlScene.AddNewNodeByClass("vtkMRMLMarkupsFiducialNode", f"CurvatureWeighted_Spaced_{model_name}")
    for idx, pt in enumerate(selected_points):
        fidNode.AddControlPoint(*pt)
        fidNode.SetNthControlPointLabel(idx, f"Semi_{idx}")
    fidNode.GetDisplayNode().SetPointLabelsVisibility(False)
    fidNode.GetDisplayNode().SetTextScale(0)

# === Batch processor ===
def process_all_models_curvature_weighted(num_landmarks=100, base_spacing=0.3):
    modelNodes = slicer.util.getNodesByClass("vtkMRMLModelNode")
    for modelNode in modelNodes:
        try:
            generate_curvature_weighted_semilandmarks(modelNode, num_landmarks=num_landmarks, base_spacing=base_spacing)
        except Exception as e:
            print(f"⚠️ Skipping {modelNode.GetName()} due to error: {e}")
    print("✅ All models processed.")

# === Run it ===
process_all_models_curvature_weighted(num_landmarks=100, base_spacing=0.3)




