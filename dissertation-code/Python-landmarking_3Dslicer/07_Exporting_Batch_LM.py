import slicer
import numpy as np
import csv
import os

def get_landmarks(markupsNode):
    """Extracts landmarks from a Markups Fiducial node and returns a dictionary of coordinates."""
    num_points = markupsNode.GetNumberOfControlPoints()
    landmarks = {}

    for i in range(num_points):
        label = markupsNode.GetNthControlPointLabel(i)
        coord = [0, 0, 0]
        markupsNode.GetNthControlPointPosition(i, coord)
        landmarks[label] = coord

    return landmarks

def export_landmarks(output_filepath):
    """Exports all landmarks (fixed + semilandmarks) for all specimens into a CSV file."""

    # Get all models and corresponding landmark nodes
    modelNodes = slicer.util.getNodesByClass("vtkMRMLModelNode")
    landmarkNodes = slicer.util.getNodesByClass("vtkMRMLMarkupsFiducialNode")

    all_landmark_labels = set()
    specimen_data = {}

    # Process each model
    for modelNode in modelNodes:
        model_name = modelNode.GetName()

        # Find the corresponding landmark node (by matching model name)
        matching_landmark_nodes = [ln for ln in landmarkNodes if model_name in ln.GetName()]

        if not matching_landmark_nodes:
            print(f"⚠️ No landmarks found for {model_name}, skipping...")
            continue

        # Collect all landmarks for the model
        specimen_landmarks = {}
        for ln in matching_landmark_nodes:
            specimen_landmarks.update(get_landmarks(ln))  # Add fixed + semilandmarks

        specimen_data[model_name] = specimen_landmarks
        all_landmark_labels.update(specimen_landmarks.keys())

    # Sort landmark labels to maintain consistent column order
    sorted_landmarks = sorted(all_landmark_labels)

    # Write to CSV
    with open(output_filepath, mode='w', newline='') as file:
        writer = csv.writer(file)
        
        # Header row
          header_flat = ["Specimen"] + [coord for lm in sorted_landmarks for coord in [f"{lm}_X", f"{lm}_Y", f"{lm}_Z"]]
        writer.writerow(header_flat)

        # Write data rows
        for specimen, landmarks in specimen_data.items():
            row = [specimen]
            for lm in sorted_landmarks:
                if lm in landmarks:
                    row.extend(landmarks[lm])  # Append X, Y, Z coordinates
                else:
                    row.extend(["", "", ""])  # Fill missing landmarks with blanks
            writer.writerow(row)

    print(f"✅ Export complete! Landmarks saved to: {output_filepath}")

# Run the export function with a chosen filename
export_landmarks(os.path.expanduser("CHANGE FOLDER"))  # Change path if needed

