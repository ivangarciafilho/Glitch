using System.Collections;
using System.Collections.Generic;
using System.Reflection.Emit;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.VFX;

public class CreativeLight : MonoBehaviour
{
    public enum renderTextureResolution
    {
        Res128,
        Res256,
        Res512,
    }
    public renderTextureResolution resolution = renderTextureResolution.Res512;
    private int resolutionRT;

    public enum camereProjection
    {
        Perspective,
        Orthographic
    }
    public camereProjection projection;
    public float orthographicSize = 5f;
    public bool autoScaleOrthographicSize = false;
    private float orthographicSizeMult = 1.0f;
    [Range(10.0f, 160.0f)]
    public float fieldOfView = 60f;


    public bool isHologram = false;
    public Camera dummyCam;
    public VisualEffect effect;
    public Material material;
    public Renderer[] renderers;

    private RenderTexture rt;

    void Start()
    {
        dummyCam.aspect = 1.0f;

        resolutionRT = 0;
        switch (this.resolution)
        {
            case renderTextureResolution.Res128:
                resolutionRT = 128;
                break;
            case renderTextureResolution.Res256:
                resolutionRT = 256;
                break;
            case renderTextureResolution.Res512:
                resolutionRT = 512;
                break;
            default:
                break;
        }

        CreateRT();
    }

    void Update()
    {
        DrawSelectedRenderers();

        effect.SetMatrix4x4("M44V", dummyCam.worldToCameraMatrix.inverse);
        effect.SetMatrix4x4("M44P", dummyCam.projectionMatrix.inverse);
        if (isHologram == true)
        {
            effect.SetInt("DepthTextureResolution", resolutionRT);
        }
        if (autoScaleOrthographicSize == true)
        {
            orthographicSizeMult = this.gameObject.transform.lossyScale.x;
        }
    }

    // Creates a new Render Texture, and sets its parameters for custom Depth Shader.
    void CreateRT()
    {
        rt = new RenderTexture(resolutionRT, resolutionRT, 32, RenderTextureFormat.RFloat, 0);
        rt.filterMode = FilterMode.Point;
        rt.wrapMode = TextureWrapMode.Clamp;
        effect.SetTexture("DepthTexture", rt);
    }

    // Draws selected renderers with a custom Depth material into a render texture
    // This texture will later be sent to the VFX Graph for particle positioning
    void DrawSelectedRenderers()
    {
        //rt.Release();
        ClearOutRenderTexture(rt);

        CommandBuffer cmd = CommandBufferPool.Get();

        if (isHologram == true)
        {
            switch (projection)
            {
                case camereProjection.Perspective:
                    dummyCam.fieldOfView = fieldOfView;
                    cmd.SetViewMatrix(dummyCam.worldToCameraMatrix);
                    cmd.SetProjectionMatrix(dummyCam.projectionMatrix);
                    effect.SetBool("PerspectiveOrOrthographic", false);
                    break;
                case camereProjection.Orthographic:
                    cmd.SetViewMatrix(dummyCam.worldToCameraMatrix);
                    cmd.SetProjectionMatrix(Matrix4x4.Ortho(-orthographicSize * orthographicSizeMult, orthographicSize * orthographicSizeMult, -orthographicSize * orthographicSizeMult, orthographicSize * orthographicSizeMult, 0.1f, 100f));
                    effect.SetBool("PerspectiveOrOrthographic", true);
                    effect.SetFloat("OrthographicSize", orthographicSize * orthographicSizeMult);
                    break;
            }
        }
        else
        {
            dummyCam.fieldOfView = fieldOfView;
            cmd.SetViewMatrix(dummyCam.worldToCameraMatrix);
            cmd.SetProjectionMatrix(dummyCam.projectionMatrix);
        }
        void ClearOutRenderTexture(RenderTexture renderTexture)
        {
            RenderTexture rt = RenderTexture.active;
            RenderTexture.active = renderTexture;
            GL.Clear(true, true, Color.clear);
            RenderTexture.active = rt;
        }

        cmd.SetRenderTarget(rt);
        for (int i = 0; i < renderers.Length; i++)
        {
            cmd.DrawRenderer(renderers[i], material, 0, 0);
        }
        Graphics.ExecuteCommandBuffer(cmd);
        cmd.Clear();
        CommandBufferPool.Release(cmd);
    }
}
