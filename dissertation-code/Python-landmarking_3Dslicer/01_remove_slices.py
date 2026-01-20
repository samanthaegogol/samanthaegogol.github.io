# Find and delete slice nodes
for sliceName in ["Red Volume Slice", "Green Volume Slice", "Yellow Volume Slice"]:
    try:
        slicer.mrmlScene.RemoveNode(slicer.util.getNode(sliceName))
        print(f"Removed {sliceName}")
    except:
        print(f"{sliceName} not found or already removed")
