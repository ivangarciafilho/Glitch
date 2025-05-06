using UnityEngine;

[RequireComponent(typeof(Camera))]
public class StopMotionRender : MonoBehaviour
{
    public int fps = 20;
    float elapsed;
    Camera cam;

    void Start()
    {
        cam = GetComponent<Camera>();
        cam.enabled = false;
    }

    void Update()
    {
        elapsed += Time.deltaTime;
        if (elapsed > 1 / fps)
        {
            elapsed = 0;
            cam.Render();
        }
    }
}
