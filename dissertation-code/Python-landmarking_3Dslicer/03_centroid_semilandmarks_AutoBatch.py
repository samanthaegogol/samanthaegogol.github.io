import slicer
import numpy as np
import vtk
import itertools

def get_landmark_position(fixedLandmarkNode, label):
    for i in range(fixedLandmarkNode.GetNumberOfControlPoints()):
        if fixedLandmarkNode.GetNthControlPointLabel(i) == label:
            return fixedLandmarkNode.GetNthControlPointPosition(i)
    raise ValueError(f"Landmark '{label}' not found.")

def place_centroid_semilandmarks(modelNode, landmarkNode, triangle_list):
    model_name = modelNode.GetName()
    polydata = modelNode.GetPolyData()

    locator = vtk.vtkPointLocator()
    locator.SetDataSet(polydata)
    locator.BuildLocator()

    node = slicer.mrmlScene.AddNewNodeByClass("vtkMRMLMarkupsFiducialNode", f"CentroidSemilandmarks_{model_name}")

    for i, triangle in enumerate(triangle_list):
        try:
            p1 = np.array(get_landmark_position(landmarkNode, triangle[0]))
            p2 = np.array(get_landmark_position(landmarkNode, triangle[1]))
            p3 = np.array(get_landmark_position(landmarkNode, triangle[2]))

            centroid = (p1 + p2 + p3) / 3.0
            closest_id = locator.FindClosestPoint(centroid)
            surface_point = polydata.GetPoint(closest_id)
            node.AddControlPoint(vtk.vtkVector3d(*surface_point))
            node.SetNthControlPointLabel(i, f"Tri_{i+1}_Center")
        except Exception as e:
            print(f"⚠️ Triangle {triangle} failed: {e}")

# === Landmark definitions ===
inner_landmarks = ["LM_5", "LM_14", "LM_15", "LM_16", "LM_17"]
outer_ring_order = [
    "LM_1", "LM_6", "LM_11", "LM_3", "LM_13", "LM_12", "LM_2",
    "LM_10", "LM_7", "LM_4", "LM_9", "LM_8"
]

triangle_defs = []

# Inner-inner-inner combinations
triangle_defs.extend(itertools.combinations(inner_landmarks, 3))

# Inner with adjacent outer arc triangles
for inner in inner_landmarks:
    for i in range(len(outer_ring_order) - 1):
        triangle_defs.append((inner, outer_ring_order[i], outer_ring_order[i + 1]))

# Base manual patches
manual_fill_triangles = [
    ("LM_13", "LM_2", "LM_12"),
    ("LM_13", "LM_15", "LM_2"),
    ("LM_13", "LM_12", "LM_15"),
    ("LM_14", "LM_1", "LM_11"),
    ("LM_14", "LM_1", "LM_6"),
    ("LM_11", "LM_6", "LM_14"),
    ("LM_17", "LM_5", "LM_14"),
    ("LM_17", "LM_5", "LM_15"),
    ("LM_17", "LM_15", "LM_12"),
    ("LM_16", "LM_3", "LM_11"),
    ("LM_16", "LM_11", "LM_5"),
    ("LM_16", "LM_5", "LM_14"),
]

# Your requested triangles
manual_fill_triangles += [
    ("LM_1", "LM_6", "LM_10"),
    ("LM_1", "LM_6", "LM_14"),
    ("LM_1", "LM_14", "LM_10"),
    ("LM_9", "LM_2", "LM_13"),
    ("LM_9", "LM_2", "LM_15"),
    ("LM_2", "LM_15", "LM_13"),
    ("LM_10", "LM_11", "LM_17"),
    ("LM_6", "LM_7", "LM_16"),
    ("LM_11", "LM_17", "LM_4"),
    ("LM_7", "LM_16", "LM_3"),
]

# Additional fills for good coverage
manual_fill_triangles += [
    ("LM_14", "LM_10", "LM_17"),
    ("LM_16", "LM_3", "LM_13"),
    ("LM_15", "LM_12", "LM_9"),
    ("LM_14", "LM_6", "LM_16"),
    ("LM_5", "LM_16", "LM_17"),
]

triangle_defs.extend(manual_fill_triangles)

# === Run across all models
modelNodes = slicer.util.getNodesByClass("vtkMRMLModelNode")
for modelNode in modelNodes:
    model_name = modelNode.GetName()
    lm_name = f"Landmarks_1to17_{model_name}"
    try:
        landmarkNode = slicer.util.getNode(lm_name)
        place_centroid_semilandmarks(modelNode, landmarkNode, triangle_defs)
        print(f"✅ {model_name}: Centroid triangle semilandmarks placed.")
    except Exception as e:
        print(f"⚠️ Skipping {model_name} - landmark node not found or failed: {e}")

# === Hide point labels
for node in slicer.util.getNodesByClass("vtkMRMLMarkupsFiducialNode"):
    if node.GetName().startswith("CentroidSemilandmarks_"):
        node.GetDisplayNode().SetPointLabelsVisibility(False)
        node.GetDisplayNode().SetTextScale(0)




