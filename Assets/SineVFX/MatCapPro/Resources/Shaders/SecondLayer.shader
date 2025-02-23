// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "SineVFX/MatCapPro/SecondLayer"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		_FinalPower("Final Power", Float) = 1
		_FinalOpacity("Final Opacity", Range( 0 , 1)) = 1
		[Toggle(_FINALOPACITYDITHERENABLED_ON)] _FinalOpacityDitherEnabled("Final Opacity Dither Enabled", Float) = 0
		[Toggle(_EDGESONLYMODEENABLED_ON)] _EdgesOnlyModeEnabled("Edges Only Mode Enabled", Float) = 0
		_Ramp("Ramp", 2D) = "white" {}
		_RampColorTint("Ramp Color Tint", Color) = (1,1,1,1)
		_RampTilingExp("Ramp Tiling Exp", Range( 0.2 , 4)) = 1
		_EdgeDetectionThickness("Edge Detection Thickness", Float) = 1
		_MaskGlowExp("Mask Glow Exp", Range( 0.2 , 8)) = 1
		_MaskGlowAmount("Mask Glow Amount", Range( 0 , 10)) = 0
		_EdgeDetectionExp("Edge Detection Exp", Range( 1 , 4)) = 1
		_EdgeGlowAmount("Edge Glow Amount", Range( 0 , 10)) = 0
		_EdgeMaskPower("Edge Mask Power", Range( -100 , 100)) = 20
		_EdgeFresnelExp("Edge Fresnel Exp", Range( 0.2 , 10)) = 2
		[Toggle(_WIREFRAMEENABLED_ON)] _WireframeEnabled("Wireframe Enabled", Float) = 0
		_WireframePower("Wireframe Power", Range( -100 , 100)) = 10
		_WireframeThickness("Wireframe Thickness", Range( 0 , 0.01)) = 10.92
		_WireframeFresnelExp("Wireframe Fresnel Exp", Range( 0.2 , 10)) = 1
		_WireframeGlowAmount("Wireframe Glow Amount", Range( 0 , 10)) = 0
		_OpacityTexture("Opacity Texture", 2D) = "white" {}
		_OpacityTextureChannel("Opacity Texture Channel", Vector) = (0,0,0,1)
		[HideInInspector] _texcoord( "", 2D ) = "white" {}

		[HideInInspector]_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		[HideInInspector]_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		[HideInInspector]_TessMin( "Tess Min Distance", Float ) = 10
		[HideInInspector]_TessMax( "Tess Max Distance", Float ) = 25
		[HideInInspector]_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		[HideInInspector]_TessMaxDisp( "Tess Max Displacement", Float ) = 25
	}

	SubShader
	{
		LOD 0

		
		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Transparent" "Queue"="Transparent" }
		
		Cull Off
		HLSLINCLUDE
		#pragma target 2.0

		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}
		
		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			
			Blend SrcAlpha OneMinusSrcAlpha , One OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			#define _RECEIVE_SHADOWS_OFF 1
			#pragma multi_compile_instancing
			#define ASE_SRP_VERSION 999999
			#define REQUIRE_OPAQUE_TEXTURE 1
			#define REQUIRE_DEPTH_TEXTURE 1

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"

			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_VERT_NORMAL
			#pragma shader_feature _WIREFRAMEENABLED_ON
			#pragma shader_feature _EDGESONLYMODEENABLED_ON
			#pragma shader_feature _FINALOPACITYDITHERENABLED_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				#ifdef ASE_FOG
				float fogFactor : TEXCOORD2;
				#endif
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _OpacityTextureChannel;
			float4 _RampColorTint;
			float4 _OpacityTexture_ST;
			float _FinalOpacity;
			float _WireframeGlowAmount;
			float _MaskGlowAmount;
			float _MaskGlowExp;
			float _FinalPower;
			float _EdgeGlowAmount;
			float _WireframeFresnelExp;
			float _WireframePower;
			float _WireframeThickness;
			float _EdgeFresnelExp;
			float _EdgeMaskPower;
			float _EdgeDetectionExp;
			float _RampTilingExp;
			float _EdgeDetectionThickness;
			float _TessPhongStrength;
			float _TessValue;
			float _TessMin;
			float _TessMax;
			float _TessEdgeLength;
			float _TessMaxDisp;
			CBUFFER_END
			sampler2D _Ramp;
			uniform float4 _CameraDepthTexture_TexelSize;
			sampler2D _OpacityTexture;


			float distortuv( float2 vsposition , float2 dir , float et )
			{
				float2 correctuv = (vsposition.xy + dir * et) / _ScreenParams.xy;
				float3 imagef = SHADERGRAPH_SAMPLE_SCENE_COLOR( correctuv.xy );
				imagef = 0.2126*imagef.r + 0.7152*imagef.g + 0.0722*imagef.b;
				return imagef;
			}
			
			float MyCustomExpression( float2 pos )
			{
				float uu = 0.0;
				float vv = 0.0;
				uu -= distortuv(pos, float2(-1.0,-1.0), _EdgeDetectionThickness);
				uu -= 2.0 * distortuv(pos, float2(-1.0,0.0), _EdgeDetectionThickness);
				uu -= distortuv(pos, float2(-1.0,1.0), _EdgeDetectionThickness);
				uu += distortuv(pos, float2(1.0,-1.0), _EdgeDetectionThickness);
				uu += 2.0 * distortuv(pos, float2(1.0,0.0), _EdgeDetectionThickness);
				uu += distortuv(pos, float2(1.0,1.0), _EdgeDetectionThickness);
				vv -= distortuv(pos, float2(-1.0,-1.0), _EdgeDetectionThickness);
				vv -= 2.0 * distortuv(pos, float2(0.0,-1.0), _EdgeDetectionThickness);
				vv -= distortuv(pos, float2(1.0,-1.0), _EdgeDetectionThickness);
				vv += distortuv(pos, float2(-1.0,1.0), _EdgeDetectionThickness);
				vv += 2.0 * distortuv(pos, float2(0.0,1.0), _EdgeDetectionThickness);
				vv += distortuv(pos, float2(1.0,1.0), _EdgeDetectionThickness);
				float output = saturate((uu*uu + vv*vv));
				return output;
			}
			
			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			
			
			VertexOutput VertexFunction ( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord3 = screenPos;
				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord4.xyz = ase_worldNormal;
				
				o.ase_texcoord5.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord4.w = 0;
				o.ase_texcoord5.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				#ifdef ASE_FOG
				o.fogFactor = ComputeFogFactor( positionCS.z );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag ( VertexOutput IN  ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif
				float4 screenPos = IN.ase_texcoord3;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float4 fetchOpaqueVal38 = float4( SHADERGRAPH_SAMPLE_SCENE_COLOR( ase_screenPosNorm.xy ), 1.0 );
				float2 pos36 = ( ase_screenPosNorm * _ScreenParams ).xy;
				float localMyCustomExpression36 = MyCustomExpression( pos36 );
				float3 ase_worldViewDir = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				ase_worldViewDir = normalize(ase_worldViewDir);
				float3 ase_worldNormal = IN.ase_texcoord4.xyz;
				float fresnelNdotV50 = dot( ase_worldNormal, ase_worldViewDir );
				float fresnelNode50 = ( 0.0 + 1.0 * pow( 1.0 - fresnelNdotV50, _EdgeFresnelExp ) );
				float clampResult40 = clamp( ( pow( localMyCustomExpression36 , _EdgeDetectionExp ) * _EdgeMaskPower * fresnelNode50 ) , -1.0 , 1.0 );
				float eyeDepth100 = (SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_screenPosNorm.xy )*( _ProjectionParams.z - _ProjectionParams.y ));
				float4 appendResult91 = (float4(_WireframeThickness , 0.0 , 0.0 , 0.0));
				float eyeDepth104 = (SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( ase_screenPosNorm + appendResult91 ).xy )*( _ProjectionParams.z - _ProjectionParams.y ));
				float4 appendResult90 = (float4(-_WireframeThickness , 0.0 , 0.0 , 0.0));
				float eyeDepth107 = (SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( appendResult90 + ase_screenPosNorm ).xy )*( _ProjectionParams.z - _ProjectionParams.y ));
				float4 appendResult84 = (float4(0.0 , _WireframeThickness , 0.0 , 0.0));
				float eyeDepth106 = (SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( appendResult84 + ase_screenPosNorm ).xy )*( _ProjectionParams.z - _ProjectionParams.y ));
				float4 appendResult85 = (float4(0.0 , -_WireframeThickness , 0.0 , 0.0));
				float eyeDepth105 = (SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( appendResult85 + ase_screenPosNorm ).xy )*( _ProjectionParams.z - _ProjectionParams.y ));
				float4 appendResult94 = (float4(_WireframeThickness , _WireframeThickness , 0.0 , 0.0));
				float eyeDepth114 = (SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( appendResult94 + ase_screenPosNorm ).xy )*( _ProjectionParams.z - _ProjectionParams.y ));
				float4 appendResult96 = (float4(-_WireframeThickness , -_WireframeThickness , 0.0 , 0.0));
				float eyeDepth112 = (SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( ase_screenPosNorm + appendResult96 ).xy )*( _ProjectionParams.z - _ProjectionParams.y ));
				float4 appendResult95 = (float4(_WireframeThickness , -_WireframeThickness , 0.0 , 0.0));
				float eyeDepth115 = (SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( ase_screenPosNorm + appendResult95 ).xy )*( _ProjectionParams.z - _ProjectionParams.y ));
				float4 appendResult93 = (float4(-_WireframeThickness , _WireframeThickness , 0.0 , 0.0));
				float eyeDepth109 = (SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( ase_screenPosNorm + appendResult93 ).xy )*( _ProjectionParams.z - _ProjectionParams.y ));
				float clampResult135 = clamp( ( ( ( eyeDepth100 - eyeDepth104 ) + ( eyeDepth100 - eyeDepth107 ) + ( eyeDepth100 - eyeDepth106 ) + ( eyeDepth100 - eyeDepth105 ) ) + ( eyeDepth100 - eyeDepth114 ) + ( eyeDepth100 - eyeDepth112 ) + ( eyeDepth100 - eyeDepth115 ) + ( eyeDepth100 - eyeDepth109 ) ) , 0.0 , 1.0 );
				float fresnelNdotV125 = dot( ase_worldNormal, ase_worldViewDir );
				float fresnelNode125 = ( 0.0 + 1.0 * pow( 1.0 - fresnelNdotV125, _WireframeFresnelExp ) );
				float clampResult127 = clamp( ( clampResult135 * _WireframePower * fresnelNode125 ) , -1.0 , 1.0 );
				#ifdef _WIREFRAMEENABLED_ON
				float staticSwitch128 = clampResult127;
				#else
				float staticSwitch128 = 0.0;
				#endif
				float clampResult42 = clamp( ( fetchOpaqueVal38.r + clampResult40 + staticSwitch128 ) , 0.0 , 1.0 );
				float2 appendResult49 = (float2(pow( clampResult42 , _RampTilingExp ) , 0.0));
				float clampResult152 = clamp( ( ( clampResult40 * _EdgeGlowAmount ) + 1.0 ) , 0.0 , 100.0 );
				float clampResult153 = clamp( ( ( staticSwitch128 * _WireframeGlowAmount ) + 1.0 ) , 0.0 , 100.0 );
				
				#ifdef _EDGESONLYMODEENABLED_ON
				float staticSwitch149 = clampResult40;
				#else
				float staticSwitch149 = 1.0;
				#endif
				float2 uv_OpacityTexture = IN.ase_texcoord5.xy * _OpacityTexture_ST.xy + _OpacityTexture_ST.zw;
				float4 break139 = ( tex2D( _OpacityTexture, uv_OpacityTexture ) * _OpacityTextureChannel );
				float clampResult143 = clamp( max( max( max( break139.r , break139.g ) , break139.b ) , break139.a ) , 0.0 , 1.0 );
				float clampResult147 = clamp( ( _FinalOpacity * clampResult143 ) , 0.0 , 1.0 );
				float2 clipScreen55 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither55 = Dither4x4Bayer( fmod(clipScreen55.x, 4), fmod(clipScreen55.y, 4) );
				float clampResult148 = clamp( ( _FinalOpacity * clampResult143 ) , 0.0 , 1.0 );
				dither55 = step( dither55, clampResult148 );
				#ifdef _FINALOPACITYDITHERENABLED_ON
				float staticSwitch57 = dither55;
				#else
				float staticSwitch57 = clampResult147;
				#endif
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = ( tex2D( _Ramp, appendResult49 ) * clampResult152 * _RampColorTint * _FinalPower * ( ( pow( fetchOpaqueVal38.r , _MaskGlowExp ) * _MaskGlowAmount ) + 1.0 ) * clampResult153 ).rgb;
				float Alpha = ( staticSwitch149 * staticSwitch57 );
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip( Alpha - AlphaClipThreshold );
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#ifdef ASE_FOG
					Color = MixFog( Color, IN.fogFactor );
				#endif

				return half4( Color, Alpha );
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0

			HLSLPROGRAM
			#define _RECEIVE_SHADOWS_OFF 1
			#pragma multi_compile_instancing
			#define ASE_SRP_VERSION 999999

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_VERT_NORMAL
			#pragma shader_feature _EDGESONLYMODEENABLED_ON
			#pragma shader_feature _FINALOPACITYDITHERENABLED_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _OpacityTextureChannel;
			float4 _RampColorTint;
			float4 _OpacityTexture_ST;
			float _FinalOpacity;
			float _WireframeGlowAmount;
			float _MaskGlowAmount;
			float _MaskGlowExp;
			float _FinalPower;
			float _EdgeGlowAmount;
			float _WireframeFresnelExp;
			float _WireframePower;
			float _WireframeThickness;
			float _EdgeFresnelExp;
			float _EdgeMaskPower;
			float _EdgeDetectionExp;
			float _RampTilingExp;
			float _EdgeDetectionThickness;
			float _TessPhongStrength;
			float _TessValue;
			float _TessMin;
			float _TessMax;
			float _TessEdgeLength;
			float _TessMaxDisp;
			CBUFFER_END
			sampler2D _OpacityTexture;


			float distortuv( float2 vsposition , float2 dir , float et )
			{
				float2 correctuv = (vsposition.xy + dir * et) / _ScreenParams.xy;
				float3 imagef = SHADERGRAPH_SAMPLE_SCENE_COLOR( correctuv.xy );
				imagef = 0.2126*imagef.r + 0.7152*imagef.g + 0.0722*imagef.b;
				return imagef;
			}
			
			float MyCustomExpression( float2 pos )
			{
				float uu = 0.0;
				float vv = 0.0;
				uu -= distortuv(pos, float2(-1.0,-1.0), _EdgeDetectionThickness);
				uu -= 2.0 * distortuv(pos, float2(-1.0,0.0), _EdgeDetectionThickness);
				uu -= distortuv(pos, float2(-1.0,1.0), _EdgeDetectionThickness);
				uu += distortuv(pos, float2(1.0,-1.0), _EdgeDetectionThickness);
				uu += 2.0 * distortuv(pos, float2(1.0,0.0), _EdgeDetectionThickness);
				uu += distortuv(pos, float2(1.0,1.0), _EdgeDetectionThickness);
				vv -= distortuv(pos, float2(-1.0,-1.0), _EdgeDetectionThickness);
				vv -= 2.0 * distortuv(pos, float2(0.0,-1.0), _EdgeDetectionThickness);
				vv -= distortuv(pos, float2(1.0,-1.0), _EdgeDetectionThickness);
				vv += distortuv(pos, float2(-1.0,1.0), _EdgeDetectionThickness);
				vv += 2.0 * distortuv(pos, float2(0.0,1.0), _EdgeDetectionThickness);
				vv += distortuv(pos, float2(1.0,1.0), _EdgeDetectionThickness);
				float output = saturate((uu*uu + vv*vv));
				return output;
			}
			
			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord2 = screenPos;
				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord3.xyz = ase_worldNormal;
				
				o.ase_texcoord4.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord3.w = 0;
				o.ase_texcoord4.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.clipPos = TransformWorldToHClip( positionWS );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float4 screenPos = IN.ase_texcoord2;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 pos36 = ( ase_screenPosNorm * _ScreenParams ).xy;
				float localMyCustomExpression36 = MyCustomExpression( pos36 );
				float3 ase_worldViewDir = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				ase_worldViewDir = normalize(ase_worldViewDir);
				float3 ase_worldNormal = IN.ase_texcoord3.xyz;
				float fresnelNdotV50 = dot( ase_worldNormal, ase_worldViewDir );
				float fresnelNode50 = ( 0.0 + 1.0 * pow( 1.0 - fresnelNdotV50, _EdgeFresnelExp ) );
				float clampResult40 = clamp( ( pow( localMyCustomExpression36 , _EdgeDetectionExp ) * _EdgeMaskPower * fresnelNode50 ) , -1.0 , 1.0 );
				#ifdef _EDGESONLYMODEENABLED_ON
				float staticSwitch149 = clampResult40;
				#else
				float staticSwitch149 = 1.0;
				#endif
				float2 uv_OpacityTexture = IN.ase_texcoord4.xy * _OpacityTexture_ST.xy + _OpacityTexture_ST.zw;
				float4 break139 = ( tex2D( _OpacityTexture, uv_OpacityTexture ) * _OpacityTextureChannel );
				float clampResult143 = clamp( max( max( max( break139.r , break139.g ) , break139.b ) , break139.a ) , 0.0 , 1.0 );
				float clampResult147 = clamp( ( _FinalOpacity * clampResult143 ) , 0.0 , 1.0 );
				float2 clipScreen55 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither55 = Dither4x4Bayer( fmod(clipScreen55.x, 4), fmod(clipScreen55.y, 4) );
				float clampResult148 = clamp( ( _FinalOpacity * clampResult143 ) , 0.0 , 1.0 );
				dither55 = step( dither55, clampResult148 );
				#ifdef _FINALOPACITYDITHERENABLED_ON
				float staticSwitch57 = dither55;
				#else
				float staticSwitch57 = clampResult147;
				#endif
				
				float Alpha = ( staticSwitch149 * staticSwitch57 );
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				return 0;
			}
			ENDHLSL
		}

	
	}
	CustomEditor "UnityEditor.ShaderGraph.PBRMasterGUI"
	Fallback "Hidden/InternalErrorShader"
	
}
/*ASEBEGIN
Version=18102
330;73;1109;594;3015.957;942.8489;1;True;False
Node;AmplifyShaderEditor.SamplerNode;137;-2143.109,974.7921;Inherit;True;Property;_OpacityTexture;Opacity Texture;20;0;Create;True;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector4Node;136;-2146.11,1177.792;Float;False;Property;_OpacityTextureChannel;Opacity Texture Channel;21;0;Create;True;0;0;False;0;False;0,0,0,1;0,0,0,1;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;138;-1823.11,1085.792;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.BreakToComponentsNode;139;-1695.11,1085.792;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.SimpleMaxOpNode;140;-1421.11,1021.792;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMaxOpNode;141;-1288.11,1106.792;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScreenParams;39;-2742.915,-503.3761;Inherit;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ScreenPosInputsNode;162;-2756.931,-875.3517;Float;False;0;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;32;-2472.947,-549.0855;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMaxOpNode;142;-1143.11,1185.792;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;51;-2318.108,-933.4071;Float;False;Property;_EdgeDetectionExp;Edge Detection Exp;11;0;Create;True;0;0;False;0;False;1;2;1;4;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;17;-1040.044,860.6466;Float;False;Property;_FinalOpacity;Final Opacity;1;0;Create;True;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;44;-2297.22,106.4527;Float;False;Property;_EdgeFresnelExp;Edge Fresnel Exp;14;0;Create;True;0;0;False;0;False;2;4;0.2;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;143;-998.2712,1186.987;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CustomExpressionNode;36;-2262.271,-562.9215;Float;False;float uu = 0.0@$float vv = 0.0@$$uu -= distortuv(pos, float2(-1.0,-1.0), _EdgeDetectionThickness)@$uu -= 2.0 * distortuv(pos, float2(-1.0,0.0), _EdgeDetectionThickness)@$uu -= distortuv(pos, float2(-1.0,1.0), _EdgeDetectionThickness)@$uu += distortuv(pos, float2(1.0,-1.0), _EdgeDetectionThickness)@$uu += 2.0 * distortuv(pos, float2(1.0,0.0), _EdgeDetectionThickness)@$uu += distortuv(pos, float2(1.0,1.0), _EdgeDetectionThickness)@$vv -= distortuv(pos, float2(-1.0,-1.0), _EdgeDetectionThickness)@$vv -= 2.0 * distortuv(pos, float2(0.0,-1.0), _EdgeDetectionThickness)@$vv -= distortuv(pos, float2(1.0,-1.0), _EdgeDetectionThickness)@$vv += distortuv(pos, float2(-1.0,1.0), _EdgeDetectionThickness)@$vv += 2.0 * distortuv(pos, float2(0.0,1.0), _EdgeDetectionThickness)@$vv += distortuv(pos, float2(1.0,1.0), _EdgeDetectionThickness)@$$float output = saturate((uu*uu + vv*vv))@$$return output@;1;False;1;True;pos;FLOAT2;0,0;In;;Float;False;My Custom Expression;False;False;0;1;0;FLOAT2;0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;145;-664.4892,901.5757;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;41;-1972.615,-754.5521;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FresnelNode;50;-1992.674,-43.23819;Inherit;True;Standard;WorldNormal;ViewDir;False;False;5;0;FLOAT3;0,0,1;False;4;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;37;-2050.921,-402.1373;Float;False;Property;_EdgeMaskPower;Edge Mask Power;13;0;Create;True;0;0;False;0;False;20;0.5;-100;100;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;43;-1686.77,-453.0898;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;144;-627.2604,1051.699;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;148;-515.4609,903.5791;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;147;-432.4609,1060.579;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.DitheringNode;55;-416.3497,713.9327;Inherit;False;0;False;3;0;FLOAT;0;False;1;SAMPLER2D;;False;2;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;150;-326.3675,494.301;Float;False;Constant;_Float0;Float 0;21;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;40;-1542.095,-454.8669;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;-1;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;149;-132.8024,507.1978;Float;False;Property;_EdgesOnlyModeEnabled;Edges Only Mode Enabled;4;0;Create;True;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;57;-176.8245,815.9179;Float;False;Property;_FinalOpacityDitherEnabled;Final Opacity Dither Enabled;3;0;Create;True;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;116;-2926.128,-2708.373;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;95;-3647.164,-1850.464;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ScreenDepthNode;105;-3199.321,-2548.828;Inherit;False;0;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;87;-3820.918,-1797.935;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;85;-3641.892,-2302.954;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ScreenPosInputsNode;89;-3732.677,-2912.831;Float;False;0;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.NegateNode;83;-3820.014,-2585.982;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;84;-3649.542,-2449.386;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;124;-2322.874,-1886.186;Float;False;Property;_WireframePower;Wireframe Power;16;0;Create;True;0;0;False;0;False;10;100;-100;100;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;96;-3648.511,-1998.626;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;76;-1034.068,126.261;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScreenDepthNode;104;-3207.198,-2854.89;Inherit;False;0;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;93;-3651.204,-1703.648;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.NegateNode;86;-3814.184,-1683.445;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;118;-2931.943,-2214.136;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScreenDepthNode;106;-3208.063,-2654.827;Inherit;False;0;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FresnelNode;125;-2467.741,-1667.678;Inherit;True;Standard;WorldNormal;ViewDir;False;False;5;0;FLOAT3;0,0,1;False;4;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;33;-937.7499,-242.4481;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;101;-3368.349,-2254.543;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GrabScreenPosition;48;-2790.767,-687.4645;Inherit;False;0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleSubtractOpNode;120;-2926.555,-2311.115;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;117;-2929.249,-2022.871;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;111;-2926.128,-2610.023;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScreenDepthNode;112;-3190.554,-2150.83;Inherit;False;0;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;132;-1356.271,449.4044;Float;False;Property;_WireframeGlowAmount;Wireframe Glow Amount;19;0;Create;True;0;0;False;0;False;0;1;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;110;-2929.762,-2910.089;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;126;-2785.417,-1553.609;Float;False;Property;_WireframeFresnelExp;Wireframe Fresnel Exp;18;0;Create;True;0;0;False;0;False;1;0.2;0.2;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;113;-2932.099,-2807.052;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScreenDepthNode;109;-3193.248,-1977.075;Inherit;False;0;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScreenDepthNode;115;-3191.901,-2061.932;Inherit;False;0;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;88;-3802.061,-2022.872;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;135;-1981.966,-2205.378;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;91;-3660.406,-2725.556;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ScreenDepthNode;107;-3209.155,-2758.64;Inherit;False;0;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;130;-880.1565,399.8644;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;108;-3362.961,-2034.993;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ScreenDepthNode;114;-3189.207,-2241.074;Inherit;False;0;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;119;-2927.902,-2121.197;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;103;-3364.308,-1924.545;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;90;-3653.913,-2589.261;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;151;212.0377,653.7679;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;122;-2159.015,-2253.208;Inherit;False;5;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;24;-838.4108,-496.1455;Inherit;True;Property;_Ramp;Ramp;5;0;Create;True;0;0;False;0;False;-1;None;7d356691fc2a479d9c6fc3646ed80697;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.StaticSwitch;128;-1596.053,-1639.777;Float;False;Property;_WireframeEnabled;Wireframe Enabled;15;0;Create;True;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;False;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;129;-1787.845,-1544.86;Float;False;Constant;_Float3;Float 3;16;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;46;-1127.881,-188.1254;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;77;-1160.17,22.26097;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;92;-3382.313,-2819.28;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ColorNode;26;-771.1932,170.1586;Float;False;Property;_RampColorTint;Ramp Color Tint;6;0;Create;True;0;0;False;0;False;1,1,1,1;1,1,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;27;-729.8708,365.6248;Float;False;Property;_FinalPower;Final Power;0;0;Create;True;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ScreenColorNode;38;-1678.181,-1082.255;Float;False;Global;_GrabScreen1;Grab Screen 1;8;0;Create;True;0;0;False;0;False;Object;-1;False;False;1;0;FLOAT2;0,0;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;97;-3371.318,-2443.668;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;146;-1045.542,758.2673;Float;False;Property;_FinalOpacityMaskPower;Final Opacity Mask Power;2;0;Create;True;0;0;False;0;False;0;1;1;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;153;-719.3429,466.9627;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;100;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;134;-2334.986,-817.1993;Float;False;Property;_EdgeDetectionThickness;Edge Detection Thickness;8;0;Create;True;0;0;True;0;False;1;2.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;82;-3814.55,-2331.366;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;79;-883.2693,-23.23907;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;81;-4215.011,-2358.871;Float;False;Property;_WireframeThickness;Wireframe Thickness;17;0;Create;True;0;0;False;0;False;10.92;0.001;0;0.01;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;127;-1846.488,-1988.884;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;-1;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;121;-2294.084,-2539.26;Inherit;False;4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScreenDepthNode;100;-3207.645,-2955.698;Inherit;False;0;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;80;-1470.869,44.36096;Float;False;Property;_MaskGlowExp;Mask Glow Exp;9;0;Create;True;0;0;False;0;False;1;1;0.2;8;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;155;-765.8466,-740.1615;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;78;-1366.869,163.9609;Float;False;Property;_MaskGlowAmount;Mask Glow Amount;10;0;Create;True;0;0;False;0;False;0;0;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;102;-3368.349,-2146.788;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;29;-1460.285,-88.53113;Float;False;Property;_EdgeGlowAmount;Edge Glow Amount;12;0;Create;True;0;0;False;0;False;0;0;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;42;-1013.708,-740.199;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;131;-1075.427,363.1304;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;49;-996.2366,-474.4093;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;99;-3370.885,-2580.519;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;45;-373.6285,-331.3239;Inherit;False;6;6;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;2;COLOR;0,0,0,0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;30;-1146.9,-742.9018;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;98;-3376.349,-2702.909;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;156;-1124.3,-869.5369;Float;False;Property;_RampTilingExp;Ramp Tiling Exp;7;0;Create;True;0;0;False;0;False;1;2.33;0.2;4;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;123;-2029.224,-2003.582;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;152;-735.0938,-219.4558;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;100;False;1;FLOAT;0
Node;AmplifyShaderEditor.GrabScreenPosition;47;-1917.793,-1078.972;Inherit;False;0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;94;-3648.511,-2152.177;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.CustomExpressionNode;133;-2285.418,-701.5486;Float;False;float2 correctuv = (vsposition.xy + dir * et) / _ScreenParams.xy@$float3 imagef = SHADERGRAPH_SAMPLE_SCENE_COLOR( correctuv.xy )@$imagef = 0.2126*imagef.r + 0.7152*imagef.g + 0.0722*imagef.b@$return imagef@;1;False;3;True;vsposition;FLOAT2;0,0;In;;Float;False;True;dir;FLOAT2;0,0;In;;Float;False;True;et;FLOAT;0;In;;Float;False;distortuv;False;True;0;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;160;668.3564,116.5736;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;True;False;False;False;False;0;False;-1;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;157;668.3564,116.5736;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;True;0;False;-1;True;True;True;True;True;0;False;-1;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;161;668.3564,116.5736;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;True;2;False;-1;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;159;668.3564,116.5736;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;158;668.3564,116.5736;Float;False;True;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;3;SineVFX/MatCapPro/SecondLayer;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;7;False;False;False;True;2;False;-1;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;0;0;True;1;5;False;-1;10;False;-1;1;1;False;-1;10;False;-1;False;False;False;True;True;True;True;True;0;False;-1;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;2;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;Hidden/InternalErrorShader;0;0;Standard;21;Surface;1;  Blend;0;Two Sided;0;Cast Shadows;0;Receive Shadows;0;GPU Instancing;1;LOD CrossFade;0;Built-in Fog;0;Meta Pass;0;DOTS Instancing;0;Extra Pre Pass;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;5;False;True;False;True;False;False;;0
WireConnection;138;0;137;0
WireConnection;138;1;136;0
WireConnection;139;0;138;0
WireConnection;140;0;139;0
WireConnection;140;1;139;1
WireConnection;141;0;140;0
WireConnection;141;1;139;2
WireConnection;32;0;162;0
WireConnection;32;1;39;0
WireConnection;142;0;141;0
WireConnection;142;1;139;3
WireConnection;143;0;142;0
WireConnection;36;0;32;0
WireConnection;145;0;17;0
WireConnection;145;1;143;0
WireConnection;41;0;36;0
WireConnection;41;1;51;0
WireConnection;50;3;44;0
WireConnection;43;0;41;0
WireConnection;43;1;37;0
WireConnection;43;2;50;0
WireConnection;144;0;17;0
WireConnection;144;1;143;0
WireConnection;148;0;145;0
WireConnection;147;0;144;0
WireConnection;55;0;148;0
WireConnection;40;0;43;0
WireConnection;149;1;150;0
WireConnection;149;0;40;0
WireConnection;57;1;147;0
WireConnection;57;0;55;0
WireConnection;116;0;100;0
WireConnection;116;1;106;0
WireConnection;95;0;81;0
WireConnection;95;1;87;0
WireConnection;105;0;97;0
WireConnection;87;0;81;0
WireConnection;85;1;82;0
WireConnection;83;0;81;0
WireConnection;84;1;81;0
WireConnection;96;0;88;0
WireConnection;96;1;88;0
WireConnection;76;0;77;0
WireConnection;76;1;78;0
WireConnection;104;0;92;0
WireConnection;93;0;86;0
WireConnection;93;1;81;0
WireConnection;86;0;81;0
WireConnection;118;0;100;0
WireConnection;118;1;112;0
WireConnection;106;0;99;0
WireConnection;125;3;126;0
WireConnection;33;0;46;0
WireConnection;101;0;94;0
WireConnection;101;1;89;0
WireConnection;120;0;100;0
WireConnection;120;1;114;0
WireConnection;117;0;100;0
WireConnection;117;1;109;0
WireConnection;111;0;100;0
WireConnection;111;1;105;0
WireConnection;112;0;102;0
WireConnection;110;0;100;0
WireConnection;110;1;104;0
WireConnection;113;0;100;0
WireConnection;113;1;107;0
WireConnection;109;0;103;0
WireConnection;115;0;108;0
WireConnection;88;0;81;0
WireConnection;135;0;122;0
WireConnection;91;0;81;0
WireConnection;107;0;98;0
WireConnection;130;0;131;0
WireConnection;108;0;89;0
WireConnection;108;1;95;0
WireConnection;114;0;101;0
WireConnection;119;0;100;0
WireConnection;119;1;115;0
WireConnection;103;0;89;0
WireConnection;103;1;93;0
WireConnection;90;0;83;0
WireConnection;151;0;149;0
WireConnection;151;1;57;0
WireConnection;122;0;121;0
WireConnection;122;1;120;0
WireConnection;122;2;118;0
WireConnection;122;3;119;0
WireConnection;122;4;117;0
WireConnection;24;1;49;0
WireConnection;128;1;129;0
WireConnection;128;0;127;0
WireConnection;46;0;40;0
WireConnection;46;1;29;0
WireConnection;77;0;38;1
WireConnection;77;1;80;0
WireConnection;92;0;89;0
WireConnection;92;1;91;0
WireConnection;38;0;162;0
WireConnection;97;0;85;0
WireConnection;97;1;89;0
WireConnection;153;0;130;0
WireConnection;82;0;81;0
WireConnection;79;0;76;0
WireConnection;127;0;123;0
WireConnection;121;0;110;0
WireConnection;121;1;113;0
WireConnection;121;2;116;0
WireConnection;121;3;111;0
WireConnection;100;0;89;0
WireConnection;155;0;42;0
WireConnection;155;1;156;0
WireConnection;102;0;89;0
WireConnection;102;1;96;0
WireConnection;42;0;30;0
WireConnection;131;0;128;0
WireConnection;131;1;132;0
WireConnection;49;0;155;0
WireConnection;99;0;84;0
WireConnection;99;1;89;0
WireConnection;45;0;24;0
WireConnection;45;1;152;0
WireConnection;45;2;26;0
WireConnection;45;3;27;0
WireConnection;45;4;79;0
WireConnection;45;5;153;0
WireConnection;30;0;38;1
WireConnection;30;1;40;0
WireConnection;30;2;128;0
WireConnection;98;0;90;0
WireConnection;98;1;89;0
WireConnection;123;0;135;0
WireConnection;123;1;124;0
WireConnection;123;2;125;0
WireConnection;152;0;33;0
WireConnection;94;0;81;0
WireConnection;94;1;81;0
WireConnection;158;2;45;0
WireConnection;158;3;151;0
ASEEND*/
//CHKSM=A1B8008E246FB84EFB367F805452AA94D472B2ED