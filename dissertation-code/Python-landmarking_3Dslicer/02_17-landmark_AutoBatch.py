import numpy as np
import slicer
import vtk

def get_mesh_points(polyData):
    return np.array([polyData.GetPoint(i) for i in range(polyData.GetNumberOfPoints())])

def compute_pca_axes(points):
    mean = np.mean(points, axis=0)
    centered = points - mean
    cov = np.cov(centered.T)
    eigvals, eigvecs = np.linalg.eigh(cov)
    order = np.argsort(eigvals)[::-1]
    return mean, eigvecs[:, order]

def find_extreme_point(points, axis_vector, direction=1):
    projections = points @ axis_vector
    idx = np.argmax(projections) if direction == 1 else np.argmin(projections)
    return points[idx]

def find_closest_point(polyData, target):
    locator = vtk.vtkPointLocator()
    locator.SetDataSet(polyData)
    locator.BuildLocator()
    closestId = locator.FindClosestPoint(target)
    return np.array(polyData.GetPoint(closestId))

def repel_outward_fixed_direction(base_point, center, polyData, direction_vector, scale=0.5):
    direction = direction_vector / np.linalg.norm(direction_vector)
    candidate = base_point + direction * np.linalg.norm(base_point - center) * scale
    return find_closest_point(polyData, candidate)

def find_deepest_central_point(polyData, center, radius_fraction=0.1):
    points = get_mesh_points(polyData)
    distances_xy = np.linalg.norm(points[:, [0, 2]] - center[[0, 2]], axis=1)
    y_depths = points[:, 1]
    max_radius = np.max(distances_xy)
    mask = distances_xy <= (radius_fraction * max_radius)

    if np.sum(mask) == 0:
        print("⚠️ No deep central point found; fallback to center.")
        return find_closest_point(polyData, center)

    combined_score = -y_depths + distances_xy * 0.5
    selected = points[mask][np.argmin(combined_score[mask])]
    print(f"🔍 LM_5 depth = {selected[1]:.2f}, XY offset = {distances_xy[mask][np.argmin(combined_score[mask])]:.2f}")
    return selected

def interpolate_and_snap(polyData, start, end, fraction):
    point = start + (end - start) * fraction
    return find_closest_point(polyData, point)

def place_landmarks(modelNode):
    polyData = modelNode.GetPolyData()
    points = get_mesh_points(polyData)
    center, axes = compute_pca_axes(points)

    long_axis = axes[:, 0]
    LM_1 = find_extreme_point(points, long_axis, -1)
    LM_2 = find_extreme_point(points, long_axis, 1)

    midpoint = LM_1 + 0.5 * (LM_2 - LM_1)
    LM_3 = repel_outward_fixed_direction(find_closest_point(polyData, midpoint + [0, 0, 1]), center, polyData, [0, 0, 1])
    LM_4 = repel_outward_fixed_direction(find_closest_point(polyData, midpoint - [0, 0, 1]), center, polyData, [0, 0, -1])

    LM_5 = find_deepest_central_point(polyData, center)

    # Rim-based arcs (6–13) with stronger repelling
    LM_6 = repel_outward_fixed_direction(interpolate_and_snap(polyData, LM_1, LM_3, 1/3), center, polyData, [0, 0, 1])
    LM_7 = repel_outward_fixed_direction(interpolate_and_snap(polyData, LM_1, LM_3, 2/3), center, polyData, [0, 0, 1])
    LM_8 = repel_outward_fixed_direction(interpolate_and_snap(polyData, LM_3, LM_2, 1/3), center, polyData, [0, 0, 1])
    LM_9 = repel_outward_fixed_direction(interpolate_and_snap(polyData, LM_3, LM_2, 2/3), center, polyData, [0, 0, 1])
    LM_10 = repel_outward_fixed_direction(interpolate_and_snap(polyData, LM_1, LM_4, 1/3), center, polyData, [0, 0, -1])
    LM_11 = repel_outward_fixed_direction(interpolate_and_snap(polyData, LM_1, LM_4, 2/3), center, polyData, [0, 0, -1])
    LM_12 = repel_outward_fixed_direction(interpolate_and_snap(polyData, LM_4, LM_2, 1/3), center, polyData, [0, 0, -1])
    LM_13 = repel_outward_fixed_direction(interpolate_and_snap(polyData, LM_4, LM_2, 2/3), center, polyData, [0, 0, -1])

    # Interior inflection landmarks (14–17)
    LM_14 = interpolate_and_snap(polyData, LM_1, LM_5, 3/5)
    LM_15 = interpolate_and_snap(polyData, LM_2, LM_5, 3/5)
    LM_16 = interpolate_and_snap(polyData, LM_3, LM_5, 3/5)
    LM_17 = interpolate_and_snap(polyData, LM_4, LM_5, 3/5)

    landmarks = [
        LM_1, LM_2, LM_3, LM_4, LM_5, LM_6, LM_7, LM_8, LM_9,
        LM_10, LM_11, LM_12, LM_13, LM_14, LM_15, LM_16, LM_17
    ]

    fiducials = slicer.mrmlScene.AddNewNodeByClass("vtkMRMLMarkupsFiducialNode", f"Landmarks_1to17_{modelNode.GetName()}")
    for i, LM in enumerate(landmarks):
        fiducials.AddControlPoint(*LM)
        fiducials.SetNthControlPointLabel(i, f"LM_{i+1}")
    print(f"✅ Landmarks 1–17 placed for {modelNode.GetName()}")

# === APPLY TO ALL MODELS ===
modelNodes = slicer.util.getNodesByClass("vtkMRMLModelNode")
for modelNode in modelNodes:
    try:
        place_landmarks(modelNode)
    except Exception as e:
        print(f"⚠️ Error on {modelNode.GetName()}: {e}")




