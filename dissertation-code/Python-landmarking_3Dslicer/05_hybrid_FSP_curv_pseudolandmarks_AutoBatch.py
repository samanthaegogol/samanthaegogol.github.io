
import slicer
import numpy as np
import vtk
from scipy.spatial.distance import cdist

def generate_clipped_curvature_fps(modelNode, num_landmarks=100, seed=42, clip_percentile=95, max_candidates=10000):
    """
    Option 3: Curvature-weighted Farthest Point Sampling with clipped curvature values and memory-safe candidate limit.
    Gives anatomically-informed, evenly spaced pseudolandmarks with balanced rim/interior coverage.
    """

    np.random.seed(seed)

    polyData = modelNode.GetPolyData()

    # Step 1: Compute Gaussian curvature
    curvatureFilter = vtk.vtkCurvatures()
    curvatureFilter.SetInputData(polyData)
    curvatureFilter.SetCurvatureTypeToGaussian()
    curvatureFilter.Update()

    curvatureMesh = curvatureFilter.GetOutput()
    curvatureArray = curvatureMesh.GetPointData().GetScalars()
    raw_curvatures = np.array([abs(curvatureArray.GetTuple1(i)) for i in range(curvatureArray.GetNumberOfTuples())])

    # Step 2: Clip extreme curvature values (e.g., top 5%)
    clip_threshold = np.percentile(raw_curvatures, clip_percentile)
    clipped_curvatures = np.clip(raw_curvatures, 0, clip_threshold)

    # Step 3: Sample surface densely
    sampler = vtk.vtkPolyDataPointSampler()
    sampler.SetInputData(polyData)
    sampler.SetDistance(0.2)  # Dense sampling
    sampler.Update()

    sampledPolyData = sampler.GetOutput()
    all_candidate_points = np.array([sampledPolyData.GetPoint(i) for i in range(sampledPolyData.GetNumberOfPoints())])
    print(f"ℹ️ Sampled {len(all_candidate_points)} candidate points from {modelNode.GetName()}")

    # Step 4: Map curvature to candidates
    locator = vtk.vtkPointLocator()
    locator.SetDataSet(curvatureMesh)
    locator.BuildLocator()
    all_candidate_curvatures = np.array([
        clipped_curvatures[locator.FindClosestPoint(pt)] for pt in all_candidate_points
    ])

    # Step 5: Memory-safe downsampling if too many candidates
    if len(all_candidate_points) > max_candidates:
        sampled_indices = np.random.choice(len(all_candidate_points), size=max_candidates, replace=False)
        candidate_points = all_candidate_points[sampled_indices]
        curvature_weights = all_candidate_curvatures[sampled_indices]
    else:
        candidate_points = all_candidate_points
        curvature_weights = all_candidate_curvatures

    # Normalize weights
    curvature_weights = (curvature_weights - np.min(curvature_weights)) / (np.ptp(curvature_weights) + 1e-8)

    # Step 6: Precompute pairwise distances
    dist_matrix = cdist(candidate_points, candidate_points)

    # Step 7: FPS with curvature weighting
    prob_weights = curvature_weights / np.sum(curvature_weights)
    first_index = np.random.choice(len(candidate_points), p=prob_weights)
    selected_indices = [first_index]

    for _ in range(1, num_landmarks):
        dists_to_selected = dist_matrix[:, selected_indices]
        min_dists = np.min(dists_to_selected, axis=1)

        selection_score = min_dists * (0.5 + 0.5 * curvature_weights)
        next_index = np.argmax(selection_score)
        selected_indices.append(next_index)

    selected_points = candidate_points[selected_indices]
    print(f"✅ {len(selected_points)} clipped-curvature pseudolandmarks placed on {modelNode.GetName()}")

    # Step 8: Place landmarks in Slicer
    fidNode = slicer.mrmlScene.AddNewNodeByClass("vtkMRMLMarkupsFiducialNode", f"ClippedFPS_100_{modelNode.GetName()}")
    for idx, pt in enumerate(selected_points):
        fidNode.AddControlPoint(*pt)
        fidNode.SetNthControlPointLabel(idx, f"Clip_{idx}")

    fidNode.GetDisplayNode().SetPointLabelsVisibility(False)
    fidNode.GetDisplayNode().SetTextScale(0)

def process_all_models(num_landmarks=100):
    """Processes all 3D models using Option 3: Clipped-curvature weighted FPS (memory-safe)."""
    modelNodes = slicer.util.getNodesByClass("vtkMRMLModelNode")
    for modelNode in modelNodes:
        print(f"🔹 Processing {modelNode.GetName()}...")
        try:
            generate_clipped_curvature_fps(modelNode, num_landmarks=num_landmarks)
        except Exception as e:
            print(f"⚠️ Skipping {modelNode.GetName()} due to error: {e}")
    print("✅ All models processed.")

# Run it
process_all_models(num_landmarks=100)

