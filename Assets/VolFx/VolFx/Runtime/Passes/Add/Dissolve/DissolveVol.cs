using System;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

//  VolFx © NullTale - https://x.com/NullTale
namespace VolFx
{
    [Serializable, VolumeComponentMenu("VolFx/Dissolve")]
    public sealed class DissolveVol : VolumeComponent, IPostProcessComponent
    {
        public static GradientValue Default 
        {
            get
            {
                var grad = new Gradient();
                grad.SetKeys(new []{new GradientColorKey(Color.black, 0f), new GradientColorKey(Color.white, 1f)}, new GradientAlphaKey[]{new GradientAlphaKey(0f, 0f), new GradientAlphaKey(0f, 0f)});
                
                return new GradientValue(grad);
            }
        }
        
        public ClampedFloatParameter         m_Weight     = new ClampedFloatParameter(0, 0, 1, false);
        public NoInterpGradientParameter     m_Color      = new NoInterpGradientParameter(Default, false);
        [HideInInspector]
        public NoInterpColorParameter        m_Mask       = new NoInterpColorParameter(new Color(0, 0, 0, 0f), false);
        public NoInterpClampedFloatParameter m_Scale      = new NoInterpClampedFloatParameter(1f, 0f, 2f, false);
        public NoInterpClampedFloatParameter m_Angle      = new NoInterpClampedFloatParameter(0f, -1f, 1f, false);
        public NoInterpVector3Parameter      m_Velocity   = new NoInterpVector3Parameter(Vector2.zero, false);
        public BoolParameter                 m_Shade      = new BoolParameter(true, false);
        public DisolveTexParameter           m_Disolve    = new DisolveTexParameter(DissolvePass.DisolveTex.Motif, false);
        public Texture2DParameter            m_Custom     = new Texture2DParameter(null);
        
        // =======================================================================
        [Serializable]
        public class DisolveTexParameter : VolumeParameter<DissolvePass.DisolveTex>
        {
            public DisolveTexParameter(DissolvePass.DisolveTex value, bool overrideState) : base(value, overrideState) { }
        }
        
        // =======================================================================
        public bool IsActive() => active && (m_Weight.value > 0);

        public bool IsTileCompatible() => false;
    }
}