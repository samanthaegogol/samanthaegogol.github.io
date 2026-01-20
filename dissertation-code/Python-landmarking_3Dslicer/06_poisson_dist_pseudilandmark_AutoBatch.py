import slicer
import numpy as np
import vtk

def generate_adaptive_semilandmarks(modelNode, num_landmarks=100):
    """Places exactly num_landmarks semilandmarks using adaptive Poisson-disc-like sampling."""
    polyData = modelNode.GetPolyData()

    # Step 1: Densely sample candidate surface points
    sampler = vtk.vtkPolyDataPointSampler()
    sampler.SetInputData(polyData)
    sampler.SetDistance(0.3)  # Smaller distance for denser base sampling
    sampler.Update()

    sampledPolyData = sampler.GetOutput()
    n_points = sampledPolyData.GetNumberOfPoints()
    candidate_points = np.array([sampledPolyData.GetPoint(i) for i in range(n_points)])
    print(f"ℹ️ Sampled {n_points} candidate surface points for {modelNode.GetName()}")

    # Step 2: Compute bounding box for scale normalization
    bounds = polyData.GetBounds()
    bbox_diag = np.linalg.norm([
        bounds[1] - bounds[0],
        bounds[3] - bounds[2],
        bounds[5] - bounds[4]
    ])
    
    # Step 3: Iteratively reduce min spacing until enough points are found
    min_distance = bbox_diag * 0.1  # Start with larger spacing
    selected_points = []
    while True:
        selected_points.clear()
        for pt in candidate_points:
            if all(np.linalg.norm(pt - sel) >= min_distance for sel in selected_points):
                selected_points.append(pt)
            if len(selected_points) == num_landmarks:
                break
        if len(selected_points) == num_landmarks:
            break
        min_distance *= 0.9  # Reduce spacing if not enough points

    # Step 4: Place points in Slicer
    fidNode = slicer.mrmlScene.AddNewNodeByClass("vtkMRMLMarkupsFiducialNode", f"Semi_{modelNode.GetName()}")
    for idx, pt in enumerate(selected_points):
        fidNode.AddControlPoint(*pt)
        fidNode.SetNthControlPointLabel(idx, f"Semi_{idx}")
    fidNode.GetDisplayNode().SetPointLabelsVisibility(False)
    fidNode.GetDisplayNode().SetTextScale(0)

    print(f"✅ {len(selected_points)} semilandmarks placed on {modelNode.GetName()}")

def process_all_models(num_landmarks=100):
    modelNodes = slicer.util.getNodesByClass("vtkMRMLModelNode")
    for modelNode in modelNodes:
        print(f"🔹 Processing {modelNode.GetName()}...")
        generate_adaptive_semilandmarks(modelNode, num_landmarks)
    print(f"✅ Finished processing {len(modelNodes)} models.")

# Run it
process_all_models(num_landmarks=100)

