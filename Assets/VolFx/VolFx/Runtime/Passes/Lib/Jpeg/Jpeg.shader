Shader "Hidden/VolFx/Jpeg"
{
    SubShader
    {
        name "Jpeg"
        Tags { "RenderPipeline" = "UniversalPipeline" }
        LOD 0

        ZTest Always
        ZWrite Off
        ZClip false
        Cull Off

        Pass // 0
        {
            HLSLPROGRAM

            #pragma vertex vert
            #pragma fragment frag

            sampler2D _MainTex;
            sampler2D _DistortionTex;
            sampler2D _ShotTex;
            sampler2D _LockTex;

            float2   _BlockSize;
            float4   _ChannelShift;
            #define  _DistortionOffset   _ChannelShift.zw

            float4  _DistortionData; // scale, mask, quantization, -
            #define _DistortionScale _DistortionData.x
            #define _Noise           _DistortionData.y
            #define _Quantization    _DistortionData.z

            float4 _FxData; // intensity, applyY, applyChroma, applyGlitch
            #define _Intensity           _FxData.x
            #define _ApplyDistortY       _FxData.y
            #define _ApplyDistortChroma  _FxData.z
            #define _ApplyDistortGlitch  _FxData.w

            struct appdata
            {
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float4 vertex : SV_POSITION;
                float2 uv : TEXCOORD0;
            };

            v2f vert(appdata v)
            {
                v2f o; o.vertex = v.vertex; o.uv = v.uv;
                return o;
            }

            // ===== RGB ↔ YCbCr Conversion =====
            float3 RGBtoYCbCr(float3 col)
            {
                float3x3 mat = float3x3(
                    0.299,     0.587,     0.114,
                   -0.168736, -0.331264,  0.5,
                    0.5,     -0.418688, -0.081312
                );
                return mul(mat, col);
            }

            float3 YCbCrtoRGB(float3 col)
            {
                float3x3 mat = float3x3(
                    1.0,  0.0,      1.402,
                    1.0, -0.34414, -0.71414,
                    1.0,  1.772,    0.0
                );
                return mul(mat, col);
            }

            /*
            float4 frag(v2f i) : SV_Target
            {
                float2 uv = i.uv;

                // ===== Distortion Sampling =====
                float4 d = tex2D(_DistortionTex, uv * _DistortionScale + _DistortionOffset).rgba;
                float2 dir = d.rg * 2.0 - 1.0;
                float strength = d.b;

                // ===== Separate UV Offsets for Luma and Chroma =====
                float2 offsetY      = dir * 0.02 * strength * _ApplyDistortY;
                float2 offsetChroma = dir * 0.02 * strength * _ApplyDistortChroma;

                float2 uvY      = uv + offsetY;
                float2 uvChroma = uv + offsetChroma;

                // ===== Block-Based Chroma Shift =====
                float2 blockShift = (frac(d.rg * 7.0) - 0.5) * _BlockSize * 1.5 * strength * _ApplyDistortChroma;
                float2 blockUV    = floor((uvChroma + blockShift) / _BlockSize) * _BlockSize;

                // ===== Luma and Chroma Sampling =====
                float3 rgbY  = tex2D(_MainTex, uvY).rgb;
                float3 yccY  = RGBtoYCbCr(rgbY);

                float3 rgbC  = tex2D(_MainTex, blockUV).rgb;
                float3 yccC  = RGBtoYCbCr(rgbC);

                // ===== Add Random Noise to Chroma Channels =====
                //float2 noise = (frac(sin(dot(blockUV , float2(12.9898,78.233) + _DistortionOffset)) * 43758.5453) - 0.5) * _Noise;
                yccC.yz += (d.ba - .5) * _Noise;

                // ===== Quantize Chroma Channels =====
                yccC.yz = floor(yccC.yz * _Quantization) / _Quantization;

                // ===== Combine Luma from Y, Chroma from C =====
                float3 yccMix = float3(yccY.x, yccC.yz);
                float3 col    = YCbCrtoRGB(yccMix);

                // ===== Glitch RGB Offset Effect =====
                float4 glitchCol;
                {
                    float2 uvG = uv + dir * 0.02 * strength * _ApplyDistortGlitch;
                    float r = tex2D(_MainTex, uvG + _ChannelShift).r;
                    float g = tex2D(_MainTex, uvG).g;
                    float b = tex2D(_MainTex, uvG - _ChannelShift).b;
                    glitchCol = float4(r, g, b, 1.0);
                }

                // ===== Block Edge Highlighting =====
                float2 fracUV = frac((uvChroma + blockShift) / _BlockSize);
                //float edge = step(0.05, abs(fracUV.x - 0.5)) + step(0.05, abs(fracUV.y - 0.5));
                float edge   = smoothstep(0.45, 0.5, abs(fracUV.x - 0.5)) + smoothstep(0.45, 0.5, abs(fracUV.y - 0.5));

                // ===== Final Mix with Intensity and Edge =====
                float mixAmt = _Intensity + edge * 0.5 * _Intensity;
                float3 final = lerp(col, glitchCol.xyz, mixAmt);

                return float4(final, 1.0);
            }*/
                
            float4 frag(v2f i) : SV_Target
            {
                float2 uv = i.uv;

                // Distortion
                float4 d = tex2D(_DistortionTex, uv * _DistortionScale + _DistortionOffset);
                float2 dir = d.rg * 2.0 - 1.0;
                float strength = d.b;

                float2 offsetY      = dir * 0.02 * strength * _ApplyDistortY;
                float2 offsetChroma = dir * 0.02 * strength * _ApplyDistortChroma;

                float2 uvY      = uv + offsetY;
                float2 uvChroma = uv + offsetChroma;

                float2 blockShift = (frac(d.rg * 7.0) - 0.5) * _BlockSize * 1.5 * strength * _ApplyDistortChroma;
                float2 blockUV    = floor((uvChroma + blockShift) / _BlockSize) * _BlockSize;

                // === Sample MainTex with alpha preservation ===
                float4 rgbaY = tex2D(_MainTex, uvY);        // RGB for luma, A preserved
                float4 rgbaC = tex2D(_MainTex, blockUV);    // RGB for chroma, A preserved
                float alphaY = (rgbaY.a + rgbaC.a) * .5;

                float3 yccY = RGBtoYCbCr(rgbaY.rgb);
                float3 yccC = RGBtoYCbCr(rgbaC.rgb);

                yccC.yz += (d.wz - .5) * _Noise;
                yccC.yz = floor(yccC.yz * _Quantization) / _Quantization;

                float3 yccMix = float3(yccY.x, yccC.yz);
                float3 col = YCbCrtoRGB(yccMix);

                // === Glitch effect with alpha support ===
                float4 glitchCol;
                {
                    float2 uvG = uv + dir * 0.02 * strength * _ApplyDistortGlitch;
                    float4 r = tex2D(_MainTex, uvG + _ChannelShift);
                    float4 g = tex2D(_MainTex, uvG);
                    float4 b = tex2D(_MainTex, uvG - _ChannelShift);

                    glitchCol = float4(r.r, g.g, b.b, dot(float3(r.a, g.a, b.a), float3(.333, .333, .333)));
                }

                float2 fracUV = frac((uvChroma + blockShift) / _BlockSize);
                float edge = step(0.05, abs(fracUV.x - 0.5)) + step(0.05, abs(fracUV.y - 0.5));
                float mixAmt = _Intensity + edge * 0.5 * _Intensity;

                float4 result = lerp(float4(col, alphaY), glitchCol, mixAmt);
                //result.a = lerp( alphaY, glitchCol.a, saturate(mixAmt));
                //result.a = lerp( alphaY, glitchCol.a, .5);
                result.a = (alphaY + glitchCol.a) * .5;
                //result.a = alphaY;

                return result;
            }
            
            ENDHLSL
        }

        Pass // 1: Scanline-Based Blending
        {
            HLSLPROGRAM

            #pragma vertex vert
            #pragma fragment frag

            sampler2D _ShotTex;
            sampler2D _LockTex;

            struct appdata
            {
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float4 vertex : SV_POSITION;
                float2 uv : TEXCOORD0;
            };

            v2f vert(appdata v)
            {
                v2f o; o.vertex = v.vertex; o.uv = v.uv;
                return o;
            }

            half luma(half3 rgb)
            {
                return dot(rgb, half3(.299, .585, .114));
            }
            
            float4 _ScanlinesDrift;

            float4 frag(v2f i) : SV_Target
            {
                float scanLine = floor(i.uv.y * _ScanlinesDrift.y);
                float mask = step(1, fmod(scanLine, 2.0));

                float4 main = tex2D(_ShotTex, i.uv);
                float4 lock = tex2D(_LockTex, i.uv);

                float4 result = lerp(main, lock, mask * (1 - luma(main.rgb)) * _ScanlinesDrift.x);
                return result;
            }

            ENDHLSL
        }
    }
}
