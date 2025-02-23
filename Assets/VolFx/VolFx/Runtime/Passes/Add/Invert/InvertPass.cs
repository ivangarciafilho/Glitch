using UnityEngine;

//  VolFx © NullTale - https://x.com/NullTale
namespace VolFx
{
    [ShaderName("Hidden/VolFx/Invert")]
    public class InvertPass : VolFx.Pass
    {
        private static readonly int s_Weight   = Shader.PropertyToID("_Weight");
        private static readonly int s_ValueTex = Shader.PropertyToID("_ValueTex");
		
		public override string ShaderName => string.Empty;
        
        [CurveRange]
        public AnimationCurve _lerp = AnimationCurve.Linear(0, 0, 1, 1);

        private                 Texture2D _adaptive;

        // =======================================================================
        public override bool Validate(Material mat)
        {
            var settings = Stack.GetComponent<InvertVol>();

            if (settings.IsActive() == false)
                return false;
            
            mat.SetFloat(s_Weight, _lerp.Evaluate(settings.m_Weight.value));
            mat.SetTexture(s_ValueTex, settings.m_Value.value.GetTexture(ref _adaptive));
            
            return true;
        }
    }
}