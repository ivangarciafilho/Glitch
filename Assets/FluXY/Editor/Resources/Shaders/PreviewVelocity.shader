﻿Shader "Fluxy/Editor/PreviewVelocity"
{
    SubShader
    {
        Cull Off
        Tags {"Queue"="Geometry" "IgnoreProjector"="True" "RenderType"="Opaque"}

        Pass
        {
            Name "Render"
            
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag

            #include "UnityCG.cginc"

            struct appdata
            {
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float2 uv : TEXCOORD0;
                float4 vertex : SV_POSITION;
            };

            sampler2D _MainTex;

            v2f vert (appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.uv = v.uv;

                return o;
            }
   
            float4 frag (v2f i) : SV_Target
            {
                float4 velocity = tex2D(_MainTex,i.uv);
                return float4(velocity.rg * 0.5 + 0.5,velocity.a * 2,1);
            }
            ENDHLSL
        }
      
    }
}
