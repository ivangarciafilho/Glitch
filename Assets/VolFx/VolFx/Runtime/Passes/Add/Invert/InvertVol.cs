using System;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

//  VolFx © NullTale - https://x.com/NullTale
namespace VolFx
{
    [Serializable, VolumeComponentMenu("VolFx/Invert")]
    public sealed class InvertVol : VolumeComponent, IPostProcessComponent
    {
        public ClampedFloatParameter m_Weight = new ClampedFloatParameter(0, 0, 1);
        public CurveParameter        m_Value  = new CurveParameter(new CurveValue(new AnimationCurve(
                                                                                        new Keyframe[]{new Keyframe(0, .5f), new Keyframe(1, .5f)})), false);
        
        // =======================================================================
        // Can be used to skip rendering if false
        public bool IsActive() => active && m_Weight.value > 0;

        public bool IsTileCompatible() => false;
    }
}