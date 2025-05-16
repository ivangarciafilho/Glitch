using System;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

//  VolFx © NullTale - https://x.com/NullTale
namespace VolFx
{
    [Serializable, VolumeComponentMenu("VolFx/Jpeg")]
    public sealed class JpegVol : VolumeComponent, IPostProcessComponent
    {
        [Header("===== Jpeg =====")]
        //[Header("===== ⊹₊ ˚‧︵‿₊୨")]
        //[Header("⊹₊ ˚‧︵‿₊୨ ")]
        public ClampedFloatParameter _intensity        = new ClampedFloatParameter(0f, -5f, 5f);
        public ClampedFloatParameter _blockSize        = new ClampedFloatParameter(7f, 0.1f, 200f);
        public ClampedFloatParameter _quantization     = new ClampedFloatParameter(15f, 2f, 32f);
        
        
        [Header("===== Distortions =====")]
        //[InspectorName("Scale")]
        public ClampedFloatParameter _distortionScale  = new ClampedFloatParameter(0f, 0f, 7f);
        
        [Header("YCbCr")]
        public ClampedFloatParameter _applyToY        = new ClampedFloatParameter(0f, 0f, 20f);
        public ClampedFloatParameter _applyToChroma   = new ClampedFloatParameter(0f, 0f, 20f);
        public ClampedFloatParameter _applyToGlitch   = new ClampedFloatParameter(0f, 0f, 20f);
        
        [Header("Animation")]
        public ClampedFloatParameter _fps        = new ClampedFloatParameter(0f, 0f, 60f);
        public ClampedFloatParameter _fpsBreak   = new ClampedFloatParameter(0f, 0f, 1f);
        public ClampedFloatParameter _quantSpread = new ClampedFloatParameter(0f, 0f, 3f);
        
        [Header("===== Additional =====")]
        public ClampedFloatParameter _scanlineDrift = new ClampedFloatParameter(0f, 0f, 1f);
        public ClampedFloatParameter _scanlineRes   = new ClampedFloatParameter(720f, 120f, 720f);
        public ClampedFloatParameter _channelShiftX = new ClampedFloatParameter(0f, -1f, 1f);
        public ClampedFloatParameter _channelShiftY = new ClampedFloatParameter(0f, -1f, 1f);
        public ClampedFloatParameter _noise         = new ClampedFloatParameter(0f, 0f, 0.33f);
        public BoolParameter         _noiseBilinear = new BoolParameter(false);
        
        // =======================================================================
        public bool IsActive() => active && _intensity != 0f;

        public bool IsTileCompatible() => true;
    }
}