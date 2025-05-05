using System;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

//  VolFx © NullTale - https://x.com/NullTale
namespace VolFx
{
    [Serializable, VolumeComponentMenu("VolFx/Sharpen")]
    public sealed class SharpenVol : VolumeComponent, IPostProcessComponent
    {
        public ClampedFloatParameter         m_Impact   = new ClampedFloatParameter(0, 0, 1);
        public NoInterpClampedFloatParameter m_Thikness = new NoInterpClampedFloatParameter(0, 0, 1);
        public CurveParameter                m_Value    = new CurveParameter(new CurveValue(AnimationCurve.Linear(0, 1, 1, 1)), false);
        public ColorParameter                m_Tint     = new ColorParameter(Color.white);
        public ClampedFloatParameter         m_OffsetX  = new ClampedFloatParameter(0, -1, 1);
        public ClampedFloatParameter         m_OffsetY  = new ClampedFloatParameter(0, -1, 1);
        
        // =======================================================================
        // Can be used to skip rendering if false
        public bool IsActive() => active && m_Impact.value > 0;

        public bool IsTileCompatible() => false;
    }
}