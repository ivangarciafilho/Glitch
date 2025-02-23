using UnityEngine;
using UnityEditor;

[CustomEditor(typeof(RampGeneratorCL))]
public class CustomRampGeneratorEditor : Editor
{
    public override void OnInspectorGUI()
    {
        DrawDefaultInspector();

        RampGeneratorCL rampscript = (RampGeneratorCL)target;

        if (rampscript.mode == RampGeneratorCL.Mode.BakeAndSaveAsTexture)
        {
            if (GUILayout.Button("Create PNG Gradient Texture"))
            {
                rampscript.BakeGradient();
                AssetDatabase.Refresh();
            }
        }               
    }
}
