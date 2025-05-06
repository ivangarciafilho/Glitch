using System.Drawing.Printing;

using UnityEditor.UI;

using UnityEngine;

public class OscillateTransform : MonoBehaviour
{
    public float speed;
    public Vector3 min;
    public Vector3 max;
    public float seedX;
    public float seedY;
    public float seedZ;

    private Vector3  defaultPosition;
    private Vector3 displacement;


    private void Awake()
    {
        defaultPosition = transform.localPosition;
        seedX = GetInstanceID();
        seedY = seedX*2;
        seedZ = seedY*2;
    }

    // Update is called once per frame
    void LateUpdate()
    {
        var time = Time.time *  speed;


        displacement.x =  Mathf.Lerp(min.x, max.x, (Mathf.PerlinNoise(time, seedX)))+defaultPosition.x;
        displacement.y = Mathf.Lerp(min.y, max.y, (Mathf.PerlinNoise(time, seedY)))+defaultPosition.y;
        displacement.z = Mathf.Lerp(min.z,max.z, (Mathf.PerlinNoise(time, seedZ)))+defaultPosition.z;

        transform.localPosition = displacement;
    }
}
