using UnityEngine;
using UnityEngine.Rendering;

namespace RadiantGI.Universal {

    [ExecuteInEditMode]
    public class RadiantShadowMap : MonoBehaviour {

        static class ShaderParams {
            public static int RadiantShadowMapColors = Shader.PropertyToID("_RadiantShadowMapColors");
            public static int RadiantShadowMapNormals = Shader.PropertyToID("_RadiantShadowMapNormals");
            public static int RadiantShadowMapWorldPos = Shader.PropertyToID("_RadiantShadowMapWorldPos");
            public static int RadiantWorldToShadowMap = Shader.PropertyToID("_RadiantWorldToShadowMap");
            public static int ClipToWorld = Shader.PropertyToID("_ClipToWorld");
            public static int ClipDir = Shader.PropertyToID("_ClipDir");
            public static int FarClipPlane = Shader.PropertyToID("_FarClipPlane");
        }

        public enum ShadowMapResolution {
            [InspectorName("64")]
            _64,
            [InspectorName("128")]
            _128,
            [InspectorName("256")]
            _256,
            [InspectorName("512")]
            _512,
            [InspectorName("1024")]
            _1024,
            [InspectorName("2048")]
            _2048
        }

        const string RADIANT_GO_NAME = "RadiantGI Capture Camera";

        public static bool installed;

        public Transform target;

        [Tooltip("The capture extents around target")]
        public float targetCaptureSize = 25;
        public ShadowMapResolution resolution = ShadowMapResolution._512;
        Light thisLight;
        public Camera captureCamera;
        Material captureMat;
        Quaternion lastRotation;
        Vector3 lastTargetPos;
        float lastCaptureSize;
        public RenderTexture rtColors, rtWorldPos, rtNormals;

        bool needShoot;

        void OnEnable () {
            thisLight = GetComponent<Light>();
            if (thisLight == null || thisLight.type != LightType.Directional) {
                Debug.LogError("Radiant Shadow Map script must be added to a directional light!");
                return;
            }
            if (captureMat == null) {
                captureMat = new Material(Shader.Find("Hidden/Kronnect/RadiantGICapture"));
            }
            SetupCamera();
            lastTargetPos = new Vector3(float.MaxValue, 0, 0);
            installed = true;
        }

        private void OnValidate () {
            targetCaptureSize = Mathf.Max(targetCaptureSize, 5);
        }

        private void OnDestroy () {
            Remove();
        }

        private void Remove () {
            installed = false;
            if (captureCamera != null && RADIANT_GO_NAME.Equals(captureCamera.name)) {
                DestroyImmediate(captureCamera.gameObject);
            }
            if (captureMat != null) {
                DestroyImmediate(captureMat);
            }
            DestroyRT(rtColors);
            DestroyRT(rtWorldPos);
            DestroyRT(rtNormals);
        }

        void SetupCamera () {
            if (captureCamera == null) {
                captureCamera = GetComponentInChildren<Camera>();
            }
            if (captureCamera != null) return;

            GameObject camGO = Instantiate(Resources.Load<GameObject>("RadiantGI/CaptureCamera"));
            camGO.name = RADIANT_GO_NAME;
            camGO.transform.SetParent(transform, false);
            captureCamera = camGO.GetComponent<Camera>();
        }

        private void LateUpdate () {
            if (thisLight == null) {
                Remove();
                return;
            }

            if (target == null) {
                target = FindTarget();
                if (target == null) return;
            }

            if (captureCamera == null) {
                SetupCamera();
                if (captureCamera == null) return;
            }

            Quaternion rotation = transform.rotation;
            if (lastCaptureSize != targetCaptureSize || lastRotation != rotation || (lastTargetPos - target.position).sqrMagnitude > 25) {
                needShoot = true;
            }

            int desiredSize = 1 << ((int)resolution + 6);
            if (rtColors == null || rtNormals == null || rtWorldPos == null || rtColors.width != desiredSize) {
                DestroyRT(rtColors);
                DestroyRT(rtNormals);
                DestroyRT(rtWorldPos);
                if (rtColors == null) {
                    RenderTextureDescriptor rtDesc = new RenderTextureDescriptor(desiredSize, desiredSize, RenderTextureFormat.ARGBHalf);
                    rtDesc.msaaSamples = 1;
                    rtDesc.useMipMap = false;
                    // create rsm color target
                    rtColors = new RenderTexture(rtDesc);
                    rtColors.Create();
                    // create rsm normals target
                    rtNormals = new RenderTexture(rtDesc);
                    rtNormals.Create();
                    // create rsm world pos target
                    rtWorldPos = new RenderTexture(rtDesc);
                    rtWorldPos.Create();
                }
                captureCamera.targetTexture = rtColors;
                needShoot = true;
            }

            if (needShoot) {
                CaptureScene();
            }
        }

        void CaptureScene () {

            // Make sure indirect light intensity > 0
            VolumeStack volume = VolumeManager.instance.stack;
            RadiantGlobalIllumination radiant = volume.GetComponent<RadiantGlobalIllumination>();
            if (radiant == null || radiant.indirectIntensity.value <= 0) return;

            // Make sure the directional light is illuminating the scene
            if (thisLight.intensity <= 0 || thisLight.transform.forward.y > 0) {
                Shader.SetGlobalTexture(ShaderParams.RadiantShadowMapColors, Texture2D.blackTexture);
                return;
            }

            needShoot = false;

            lastRotation = transform.rotation;
            lastTargetPos = target.position;
            lastCaptureSize = targetCaptureSize;
            float farClipPlane = captureCamera.farClipPlane;
            Vector3 targetPosition = target != null ? target.transform.position : Vector3.zero;
            captureCamera.transform.localRotation = Quaternion.identity;
            captureCamera.transform.localPosition = targetPosition + new Vector3(0, 0, farClipPlane * -0.5f);
            captureCamera.orthographicSize = targetCaptureSize;
            captureCamera.Render();

            captureMat.SetMatrix(ShaderParams.ClipToWorld, captureCamera.cameraToWorldMatrix * captureCamera.projectionMatrix.inverse);
            captureMat.SetVector(ShaderParams.ClipDir, transform.forward);
            captureMat.SetFloat(ShaderParams.FarClipPlane, farClipPlane);

            Graphics.Blit(rtColors, rtWorldPos, captureMat, 0);

            Shader.SetGlobalTexture(ShaderParams.RadiantShadowMapWorldPos, rtWorldPos);
            Graphics.Blit(rtColors, rtNormals, captureMat, 1);

            Shader.SetGlobalTexture(ShaderParams.RadiantShadowMapColors, rtColors);
            Shader.SetGlobalTexture(ShaderParams.RadiantShadowMapNormals, rtNormals);
            Shader.SetGlobalMatrix(ShaderParams.RadiantWorldToShadowMap, captureCamera.projectionMatrix * captureCamera.worldToCameraMatrix);
        }

        Transform FindTarget () {
            Camera cam = Camera.main;
            if (cam != null) return cam.transform;
            return null;
        }

        void DestroyRT (RenderTexture rt) {
            if (rt == null) return;
            rt.Release();
            DestroyImmediate(rt);
        }

    }

}
