using UnityEngine;
using UnityEngine.Experimental.Rendering;
using UnityEngine.Rendering;

//  VolFx © NullTale - https://x.com/NullTale
namespace VolFx
{
    [ShaderName("Hidden/VolFx/Jpeg")]
    public class JpegPass : VolFx.Pass
    {
        private static readonly int s_Intensity       = Shader.PropertyToID("_Intensity");
        private static readonly int s_BlockSize       = Shader.PropertyToID("_BlockSize");
        private static readonly int s_ChannelShift    = Shader.PropertyToID("_ChannelShift");
        private static readonly int s_DistortionTex   = Shader.PropertyToID("_DistortionTex");
        private static readonly int s_DistortionScale = Shader.PropertyToID("_DistortionScale");
        private static readonly int s_DistortionMask  = Shader.PropertyToID("_DistortionMask");
        private static readonly int s_ScanlinesDrift  = Shader.PropertyToID("_ScanlinesDrift");
        
        private static readonly int s_DistortionData = Shader.PropertyToID("_DistortionData");
        private static readonly int s_FxData = Shader.PropertyToID("_FxData");

        private bool      _shotFirstTime;
        private float     _fpsLast;
        private Vector2   _distortionOffset;
        private Texture2D _distortionMap;
        private float     _quantSpread;
        
        private readonly int _shotTexID = Shader.PropertyToID("_ShotTex");
        private readonly int _lockTexID = Shader.PropertyToID("_LockTex");

        private RenderTarget _shotTex;
        private RenderTarget _lockTex;

        private float _scanlines;

        public override string ShaderName { get; } = string.Empty;

        // =======================================================================
        public override void Init()
		{
            _shotTex = new RenderTarget().Allocate($"{name}_shot_rt");
			_lockTex = new RenderTarget().Allocate($"{name}_lock_rt");
            
            _shotFirstTime = true;
            
            _fpsLast = -1f;
        }
        
        public override bool Validate(Material mat)
        {
            var settings = Stack.GetComponent<JpegVol>();
            if (settings == null || !settings.IsActive())
                return false;

            if (Time.time - _fpsLast > (1f / settings._fps.value) || UnityEngine.Random.value < settings._fpsBreak.value)
            {
                _fpsLast = Time.time;
                _distortionOffset = new Vector2(UnityEngine.Random.value * 37f, UnityEngine.Random.value * 37f);
                _quantSpread = settings._quantSpread.value * Random.value - settings._quantSpread.value * .5f;
            }

            var blockSize = new Vector4(settings._blockSize.value / Screen.width, settings._blockSize.value / Screen.width);
            var channelShift = new Vector4(settings._channelShiftX.value, settings._channelShiftY.value);
            
            
            mat.SetVector(s_BlockSize, blockSize);
            mat.SetVector(s_ChannelShift, new Vector4(channelShift.x, channelShift.y, _distortionOffset.x, _distortionOffset.y));

            if (_distortionMap == null)
                _distortionMap = _generateDistortionMap(256);
            
            _distortionMap.filterMode = settings._noiseBilinear.value ? FilterMode.Bilinear : FilterMode.Point;
            
            mat.SetTexture(s_DistortionTex, _distortionMap);

            mat.SetVector(s_DistortionData, new Vector4(
                              settings._distortionScale.value, 
                              settings._noise.value, 
                              Mathf.Max(settings._quantization.value + _quantSpread, 1f),
                              0f));
            
            mat.SetVector(s_FxData, new Vector4(settings._intensity.value + 1f, settings._applyToY.value, settings._applyToChroma.value, settings._applyToGlitch.value));

            _scanlines =  settings._scanlineDrift.value;
            mat.SetVector(s_ScanlinesDrift, new Vector4(settings._scanlineDrift.value, settings._scanlineRes.value));
            
            return true;
        }

        // =======================================================================
        public override void Init(VolFx.InitApi initApi)
        {
            initApi.Allocate(_lockTex, Screen.width, Screen.height, GraphicsFormat.R8G8B8A8_UNorm);
            initApi.Allocate(_shotTex, Screen.width, Screen.height, GraphicsFormat.R8G8B8A8_UNorm);
        }
        
        public override void Invoke(RTHandle source, RTHandle dest, VolFx.CallApi callApi)
        {
            if (callApi.CamType != CameraType.Game)
            {
                callApi.Blit(source, dest);
                return;
            }
            
            if (_scanlines <= 0f)
            {
                callApi.Blit(source, dest, _material, 0);
                _shotFirstTime = true;
                return;
            }
            
            
            callApi.Blit(source, _shotTex.Handle, _material, 0);
            callApi.Mat.SetTexture(_shotTexID, _shotTex.Handle);
            
            // copy content to lock texture first, if needed 
            if (_shotFirstTime)
            {
                _shotFirstTime = false;
                callApi.Blit(_shotTex.Handle, _lockTex.Handle);
                callApi.Mat.SetTexture(_lockTexID, _lockTex.Handle);
            }
            callApi.Mat.SetTexture(_lockTexID, _lockTex.Handle);

            callApi.Blit(_shotTex.Handle, dest, _material, 1);
            callApi.Blit(_shotTex.Handle, _lockTex.Handle);
        }

        // =======================================================================
        public static Texture2D _generateDistortionMap(int size = 256)
        {
            var tex = new Texture2D(size, size, TextureFormat.RGBA32, false);
            tex.wrapMode   = TextureWrapMode.Repeat;
            tex.filterMode = FilterMode.Point;

            for (var y = 0; y < size; y++)
            for (var x = 0; x < size; x++)
            {
                var fx = (float)x / size;
                var fy = (float)y / size;

                // aliasing pattern + random noise
                var aliasX = Mathf.Sin(fx * 32 * Mathf.PI) * 0.5f + 0.5f;
                var aliasY = Mathf.Cos(fy * 24 * Mathf.PI) * 0.5f + 0.5f;

                var r = Mathf.Lerp(Random.value, aliasX, 0.5f);
                var g = Mathf.Lerp(Random.value, aliasY, 0.5f);
                var b = Mathf.PerlinNoise(fx * 8, fy * 8); // mask pattern
                
                var a = UnityEngine.Random.value;
                
                tex.SetPixel(x, y, new Color(r, g, b, a));
            }

            tex.Apply();
            return tex;
        }
    }
}