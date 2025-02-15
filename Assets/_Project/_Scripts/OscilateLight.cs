using RadiantGI.Universal;

using System.Collections;
using System.Collections.Generic;

using Unity.Mathematics;
using Unity.VisualScripting;

using UnityEngine;

public class OscilateLight : MonoBehaviour
{
    public RadiantVirtualEmitter itsLight;
    public Vector2 intensityRange;
    public int octaves;
    public float frequency;
    public float frequencyOffset;
    public float[] seeds;

    public void OnEnable()
    {
        seeds= new float[ octaves ];
        seeds[0] = gameObject.GetInstanceID();

        for (int i = 1; i < octaves; i++)
            seeds[i] = seeds[i-1] * Mathf.PI;
    }


    // Update is called once per frame
    void Update()
    {
        var time = Time.time * frequency;
        var noise = 0f;
        for (int i = 0; i < octaves; i++)
        {
            var speed = time * (1 + (i * frequencyOffset));
            noise += Mathf.PerlinNoise(speed, seeds[i]);
        }

        noise = noise / octaves;
        noise = Mathf.Lerp(intensityRange.x, intensityRange.y, noise ) ;

        itsLight.intensity = noise;
    }
}
