Shader "ZTY/LightingWater/URP"
{
    Properties
    {
        [Title(Surface)]
        [Main(Basecolor, _, off, off)]
        [Space(10)]
        _groupBasecolor ("Basecolor", float) = 1
        [Space(5)]
        [Sub(Basecolor)]_shallowColor ("Shallow Color", Color) = (0.6, 0.8, 0.8, 0.0)
        [Sub(Basecolor)]_deepColor ("Deep Color", Color) = (0.1, 0.26, 0.3, 0.0)
        [Sub(Basecolor)]_deepRange ("Deep Range", Range(0.0, 10.0)) = 1.0
        [Sub(Basecolor)]_deepOpacity ("Deep Opacity", Range(0.0, 1.0)) = 0.95

        [Main(Specular, _, off, off)]
        _groupSpecular ("Specular", float) = 1
        [Space(5)]
        [Sub(Specular)]_specularColor ("Specular Color", Color) = (0.8, 0.9, 1.0, 0.0)
        [Sub(Specular)]_specularIntensity ("Specular Intensity", Range(0.0, 3.0)) = 1.0
        [Sub(Specular)]_waterRoughness ("Roughness", Range(0.0, 1.0)) = 0.1
        [Sub(Specular)]_waterMetallic ("Metallic", Range(0.0, 1.0)) = 0.04

        [Main(Reflection, _, off, off)]
        _groupPlanar ("Reflection", float) = 1
        [Space(5)]
        [Sub(Reflection)]_reflectionDisort ("Reflection Disort", Range(0.0, 1.0)) = 0.2
        [Sub(Reflection)]_reflectionIntensity ("Reflection Intensity", Range(0.0, 1.0)) = 1.0
        // [KeywordEnum(Probe, Planar)]_reflectionMode ("Reflection Mode", float) = 0

        [Main(Wave, _WAVE, on)]
        _groupWave ("Wave", float) = 1
        [Space(5)]
        // [Sub(Wave)][Toggle(_WAVE)]_WAVE ("Wave On", int) = 1
        [Sub(Wave)]_waveDirection ("Wave Direction", vector) = (1.0, 1.0, 0.0, 0.0)
        [Sub(Wave)]_waveSpeed ("Wave Speed", float) = 2.0
        [Sub(Wave)]_waveScale ("Wave Scale", Range(0.0, 1.0)) = 0.9
        [Sub(Wave)]_waveHeight ("Wave Height", Range(-2.0, 10.0)) = 1.5
        [Sub(Wave)]_waveDetailScale ("Wave Detail Scale", vector) = (0.5, 0.5, 0.5, -1.0)
        [Sub(Wave)]_waveNormalStr ("wave Normal Str", float) = 5.0
        [Sub(Wave)]_waveFadeStart ("Wave Fade Start", float) = 5.0
        [Sub(Wave)]_waveFadeEnd ("Wave Fade End", float) = 150.0

        [Main(Crest, _, off, off)]
        _groupCrest ("Crest", float) = 1
        [Space(5)]
        [Sub(Crest)]_waveCrestColor ("Crest Color", Color) = (0.2, 0.5, 0.5, 0.0)
        [Sub(Crest)]_crestRadius ("Crest Radius", Range(0.0, 4.0)) = 1.5
        [Sub(Crest)]_crestIntensity ("Crest Intensity", Range(0.0, 1.0)) = 0.1

        [Main(Ripple, _USERIPPLE, on)]
        _groupRipple ("Ripple", float) = 1
        [Space(5)]
        // [Toggle(_USERIPPLE)]_RIPPLEON ("Ripple On", int) = 1
        [Tex(Ripple)][Normal][NoScaleOffset] _ripple ("Ripple Map", 2D) = "bump" { }
        [Sub(Ripple)]_smallRippleParamsA ("A Tilling(XY) Intensity(Z)", vector) = (0.01, 0.01, 0.7, 0.0)
        [Sub(Ripple)]rippleDirectionA ("DirectionA Center(XY) Angle(Z) Speed(W)", vector) = (0.0, 0.0, 0.0, 0.1)
        [Sub(Ripple)]_smallRippleParamsB ("B Tilling(XY) Intensity(Z)", vector) = (0.03, 0.03, 0.8, 0.0)
        [Sub(Ripple)]rippleDirectionB ("DirectionB Center(XY) Angle(Z) Speed(W)", vector) = (0.0, 0.0, 0.0, 0.1)
        [Sub(Ripple)]_rippleFadeOut ("Ripple Fade Out", Range(1.0, 10.0)) = 2.0

        [Main(Detail, _USEDETAILRIPPLE, on)]
        _groupDetail ("Detail Ripple", float) = 1
        [Space(5)]
        // [Toggle()]_DETAILRIPPLEON ("Detial On", int) = 0
        [Tex(Detail)][Normal][NoScaleOffset] _detialRipple ("Detial Map", 2D) = "bump" { }
        [Sub(Detail)]_detialRippleParamsA ("A Tilling(XY) Intensity(Z)", vector) = (0.03, 0.03, 1.0, 0.0)
        [Sub(Detail)]_detialDirectionA ("DirectionA Center(XY) Angle(Z) Speed(W)", vector) = (0.0, 0.0, 0.0, 0.1)
        [Sub(Detail)]_detialRippleParamsB ("B Tilling(XY) Intensity(Z)", vector) = (0.03, 0.03, 0.07, 0.0)
        [Sub(Detail)]_detialDirectionB ("DirectionB Center(XY) Angle(Z) Speed(W)", vector) = (0.0, 0.0, 0.0, 0.1)
        [Sub(Detail)]_detailNormalSpread ("Detial Spread", Range(1.0, 5.0)) = 1.0

        [Title(Under)]
        [Main(Refraction, _USEREFRACTION, on)]
        [Space(10)]
        _groupRefraction ("Refraction", float) = 1
        [Space(5)]
        // [Toggle()]_REFRACTIONON ("Refraction On", int) = 1
        [Sub(Refraction)]_refractionIntensity ("Refraction Intensity", Range(0.0, 1.0)) = 0.5

        [Main(Caustics, _USECAUSTICS, on)]
        _groupCaustics ("Caustics", float) = 1
        [Space(5)]
        // [Toggle(_USECAUSTICS)]_CAUSTICSON ("Caustics On", int) = 1
        [Sub(Caustics)]_causticsColor ("Caustics Color", Color) = (1.0, 1.0, 1.0, 0.0)
        [Tex(Caustics)][NoScaleOffset] _causticsMap ("Caustics Map", 2D) = "white" { }
        [Sub(Caustics)]_causticsParams ("Tilling(XY) Intensity(Z) Speed(W)", vector) = (0.05, 0.05, 0.3, 0.07)
        [Sub(Caustics)]_causticsDisort ("Disort", Range(0.0, 3.0)) = 0.5
        [Sub(Caustics)]_causticsFadeOut ("FadeOut", Range(1.0, 10.0)) = 2.0
        
        [Header(Lighting Debug)]
        [Space(10)]
        [KeywordEnum(DirectDiffuse, DirectSpecular, IndirectDiffuse, IndirectSpecular, DirectLighting, IndirectLighting, All)]_LightingCheck ("Lighting Check", float) = 6
    }

    SubShader
    {
        Tags
        {
            "RenderPipeline" = "UniversalPipeline"
            "RenderType" = "Transparent"
            "Queue" = "Transparent-100"
            "IgnoreProjector" = "True"
            "ShaderModel" = "4.5"
        }
        Blend One Zero
        Pass
        {
            Tags
            {
                "LightMode" = "UniversalForward"
            }
            Name "Water"
            HLSLPROGRAM
            #pragma target 4.5
            #pragma vertex LitPassVertex
            #pragma fragment LitPassFragment
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "./Input_Wave.hlsl"
            #include "./Input_Color.hlsl"
            #include "./WaterLighting.hlsl"
            // Keywords
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
            #pragma multi_compile _ _SURFACE_TYPE_TRANSPARENT
            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            // #pragma multi_compile_fragment _ _REFLECTION_PROBE_BLENDING
            // #pragma multi_compile_fragment _ _REFLECTION_PROBE_BOX_PROJECTION

            #pragma shader_feature_local _USECREST
            #pragma shader_feature_local _USERIPPLE
            #pragma shader_feature_local _USEDETAILRIPPLE
            #pragma shader_feature_local _WAVE
            #pragma shader_feature_local _USEREFRACTION
            #pragma shader_feature_local _USECAUSTICS

            #pragma shader_feature_local _LIGHTINGCHECK_DIRECTDIFFUSE _LIGHTINGCHECK_DIRECTSPECULAR _LIGHTINGCHECK_INDIRECTDIFFUSE _LIGHTINGCHECK_INDIRECTSPECULAR _LIGHTINGCHECK_DIRECTLIGHTING _LIGHTINGCHECK_INDIRECTLIGHTING _LIGHTINGCHECK_ALL
            #pragma exclude_renderers gles gles3 glcore
            // #pragma enable_d3d11_debug_symbols

            struct Attributes
            {
                float4 pos_vertex : POSITION;
                float3 normal_vertex : NORMAL;
                float4 tangent_vertex : TANGENT;
            };

            struct Varyings
            {
                float4 pos_clip : SV_POSITION;
                float3 pos_view : TEXCOORD0;
                float3 pos_world : TEXCOORD1;
                float3 normal_world : TEXCOORD2;
                float4 tangent_world : TEXCOORD3;
                float3 bitangent_world : TEXCOORD4;
                float4 shadowCoord : TEXCOORD5;
                float3 pos_offset : TEXCOORD6;
            };

            CBUFFER_START(UnityPerMaterial)
                float4 _shallowColor, _deepColor, _specularColor, _waveCrestColor, _waveDirection, _waveDetailScale, _causticsColor, _causticsParams,
                _smallRippleParamsA, rippleDirectionA, _smallRippleParamsB, rippleDirectionB, _detialRippleParamsA, _detialDirectionA, _detialRippleParamsB, _detialDirectionB;
                float _deepRange, _deepOpacity, _waterRoughness, _waterMetallic, _specularIntensity, _reflectionDisort, _reflectionIntensity, _rippleFadeOut, _refractionIntensity, _detailNormalSpread,
                _crestIntensity, _causticsFadeOut, _crestRadius, _waveSpeed, _waveScale, _waveHeight, _waveNormalStr, _waveFadeStart, _waveFadeEnd, _causticsDisort;
            CBUFFER_END
            TEXTURE2D(_causticsMap);                  SAMPLER(sampler_causticsMap);
            TEXTURE2D(_PlanarReflectionTexture);      SAMPLER(sampler_PlanarReflectionTexture);
            TEXTURE2D(_ripple);                       SAMPLER(sampler_ripple);
            TEXTURE2D(_detialRipple);                 SAMPLER(sampler_detialRipple);
            TEXTURE2D(_CameraDepthTexture);           SAMPLER(sampler_CameraDepthTexture);
            TEXTURE2D(_CameraOpaqueTexture);          SAMPLER(sampler_CameraOpaqueTexture);
            
            Varyings LitPassVertex(Attributes input)
            {
                Varyings output = (Varyings)0;

                ////////////////////////////
                //    Normal Transform    //
                ////////////////////////////
                VertexNormalInputs normalInput = GetVertexNormalInputs(input.normal_vertex, input.tangent_vertex);
                output.tangent_world.xyz = normalInput.tangentWS;
                output.bitangent_world = normalInput.bitangentWS;
                output.normal_world = normalInput.normalWS;

                //////////////////////////////
                //    Position Transform    //
                //////////////////////////////
                VertexPositionInputs vertexInput = GetVertexPositionInputs(input.pos_vertex.xyz);
                output.pos_world = vertexInput.positionWS;
                output.pos_view = vertexInput.positionVS;
                output.shadowCoord = output.shadowCoord;

                ///////////////////////
                //    Water Wave     //
                ///////////////////////
                output.pos_offset = float3(0.0, 0.0, 0.0);
                #ifdef _WAVE
                    float2 speed_Wave = _Time.y * _waveSpeed * normalize(_waveDirection).xy;
                    GetWaveInfo(output.pos_world.xz, speed_Wave, _waveDetailScale, _waveScale, _waveHeight, _waveNormalStr, _waveFadeStart, _waveFadeEnd, output.pos_offset, output.normal_world);
                #endif
                input.pos_vertex.xyz += output.pos_offset;
                output.pos_world = TransformObjectToWorld(input.pos_vertex.xyz);

                ///////////////////////////
                //    Vertex Outputs     //
                ///////////////////////////
                output.pos_clip = TransformObjectToHClip(input.pos_vertex.xyz);
                return output;
            }

            float4 LitPassFragment(Varyings input) : SV_Target
            {
                //////////////////////////
                ///    Input Dates     ///
                //////////////////////////
                float3 worldNormal = input.normal_world;
                float3 worldPosition = input.pos_world;
                float3 cameraPosition = normalize(_WorldSpaceCameraPos.xyz - worldPosition);
                float4 screenPosition = input.pos_clip / _ScreenParams;
                
                ///////////////////////
                //    Get Shadow     //
                ///////////////////////
                float4 shadowUV = input.shadowCoord;
                #if defined(_MAIN_LIGHT_SHADOWS_SCREEN) && !defined(_SURFACE_TYPE_TRANSPARENT)
                    float4 positionCS = TransformWorldToHClip(worldPosition);
                    shadowUV = ComputeScreenPos(positionCS);
                #else
                    shadowUV = TransformWorldToShadowCoord(worldPosition);
                #endif
                Light light = GetMainLight(shadowUV, worldPosition, float4(1.0, 1.0, 1.0, 1.0));
                float3 light_Dir = light.direction;
                float3 light_Color = light.color;
                real shadowAttenuation = light.shadowAttenuation;

                ///////////////////////
                //    Water Wave     //
                ///////////////////////
                float3 wavePosition = 0.0;
                #ifdef _WAVE
                    wavePosition = input.pos_offset;
                    float2 speed_Wave = _Time.y * _waveSpeed * normalize(_waveDirection).xy;
                    GetWaveInfo(worldPosition.xz, speed_Wave, _waveDetailScale, _waveScale, _waveHeight, _waveNormalStr, _waveFadeStart, _waveFadeEnd, wavePosition, worldNormal);
                #endif

                ///////////////////////
                //    Water alpha    //
                ///////////////////////
                float depth_water = GetWaterDepth(_CameraDepthTexture, sampler_CameraDepthTexture, screenPosition.xy, input.pos_view.z, _deepRange, _deepOpacity);

                ////////////////////////
                //    Water Ripple    //
                ////////////////////////
                // Ripple
                float3 rippleNormal = float3(0.0, 0.0, 1.0);
                #ifdef _USERIPPLE
                    _smallRippleParamsA.z *= saturate(wavePosition.y + 0.5) * depth_water;
                    _smallRippleParamsB.z *= saturate(wavePosition.y + 0.5) * depth_water;
                    rippleNormal = GetRippleNormal(_ripple, sampler_ripple, worldPosition.xz, _smallRippleParamsA, _smallRippleParamsB, rippleDirectionA, rippleDirectionB);
                #endif
                // Detail
                float3 detailrippleNormal = float3(0.0, 0.0, 1.0);
                #ifdef _USEDETAILRIPPLE
                    float detailFactor = dot(worldNormal, cameraPosition);
                    detailFactor = saturate(pow(abs(detailFactor), _detailNormalSpread));
                    _detialRippleParamsA.z *= saturate(wavePosition.y + 0.5) * depth_water;
                    _detialRippleParamsB.z *= saturate(wavePosition.y + 0.5) * depth_water;
                    detailrippleNormal = GetRippleNormal(_detialRipple, sampler_detialRipple, worldPosition.xz, _detialRippleParamsA, _detialRippleParamsB, _detialDirectionA, _detialDirectionB);
                    detailrippleNormal = lerp(float3(0.0, 0.0, 1.0), detailrippleNormal, detailFactor);
                #endif
                // Transform normal from tangent to world
                float3 normalBlendTS = NormalBlending_ReorientedNormalMapping(rippleNormal, detailrippleNormal);
                float fadeOutFactor = GetFresnelFactor(worldNormal, cameraPosition, _rippleFadeOut, 16.0);
                float3x3 matrix_TBN = float3x3(input.tangent_world.xyz, input.bitangent_world, input.normal_world);
                float3 normalBlendWS = normalize(mul(normalBlendTS, matrix_TBN));
                normalBlendWS = lerp(worldNormal, normalBlendWS, fadeOutFactor);
                // FinalNormal
                float3 finalWorldNormal = normalize(normalBlendWS + worldNormal);
                finalWorldNormal = lerp(worldNormal, finalWorldNormal, depth_water);

                ///////////////////////
                //    Crest Color    //
                ///////////////////////
                float3 crestColor = 0;
                #ifdef _WAVE
                    float ndotl = max(0.0, dot(finalWorldNormal, light_Dir));
                    float waveCrestMask = max(0.0, _crestIntensity * saturate(pow(wavePosition.y + _crestRadius, 2)));
                    waveCrestMask *= waveCrestMask * (1 - ndotl);
                    crestColor = _waveCrestColor.xyz * waveCrestMask * depth_water * depth_water;
                #endif

                //////////////////////////
                //    Caustics Color    //
                //////////////////////////
                float3 color_caustics = 0.0;
                #ifdef _USECAUSTICS
                    float4 depthToWorldPosition = ReconstructWorldPosition(_CameraDepthTexture, sampler_CameraDepthTexture, screenPosition.xy, input.pos_view);
                    float3 causticsRefractionUV = GetRippleNormal(_ripple, sampler_ripple, depthToWorldPosition.xz * 10, _smallRippleParamsA * 0.1, _smallRippleParamsB * 0.1, rippleDirectionA, rippleDirectionB);
                    causticsRefractionUV = normalize(mul(causticsRefractionUV, matrix_TBN));
                    float causticsMask = (1 - depth_water) * depth_water * depth_water * (1 - depth_water) ;
                    color_caustics = GetCausticsColor(_causticsColor.xyz, causticsMask, _causticsMap, sampler_causticsMap, depthToWorldPosition.xz, _causticsParams, causticsRefractionUV.xz, _causticsDisort);
                    float causticsFactor = GetFresnelFactor(finalWorldNormal, cameraPosition, _causticsFadeOut, 3);
                    color_caustics *= causticsFactor;
                    color_caustics = lerp(color_caustics * 0.25, color_caustics, shadowAttenuation);
                #endif
                ////////////////////////////
                //    Refraction Color    //
                ////////////////////////////
                float3 color_Refraction = 0.0;
                #ifdef _USEREFRACTION
                    // float4 depthToWorldPosition2 = ReconstructWorldPosition(_CameraDepthTexture, sampler_CameraDepthTexture, screenPosition.xy, input.pos_view);
                    // float3 underRefractionUV = GetRippleNormal(_ripple, sampler_ripple, depthToWorldPosition2.xz * 8, _smallRippleParamsA * 0.1, _smallRippleParamsB * 0.1, rippleDirectionA, rippleDirectionB);
                    float2 refractionUV = screenPosition.xy + (finalWorldNormal.xz * _refractionIntensity * depth_water);
                    color_Refraction = GetRefractionColor(refractionUV, _CameraDepthTexture, sampler_CameraDepthTexture, input.pos_view.z, _CameraOpaqueTexture, sampler_CameraOpaqueTexture, screenPosition.xy);
                #endif
                ///////////////////////
                //    Color Blend    //
                ///////////////////////
                // Diffuse
                float3 waterDiffuse_Direct = lerp(_shallowColor.xyz * color_Refraction, _deepColor.xyz, depth_water);
                waterDiffuse_Direct = lerp(waterDiffuse_Direct, 0.001, _waterMetallic);
                #ifdef _WAVE
                    waterDiffuse_Direct += crestColor;
                #endif
                #ifdef _USECAUSTICS
                    waterDiffuse_Direct += color_caustics;
                #endif
                // Specular
                float3 waterSpecular_Direct = lerp(_specularColor.xyz, waterDiffuse_Direct, _waterMetallic);
                // Lighting
                float3 water_DirectLighting = WaterDirectLightingBRDF(waterDiffuse_Direct, waterSpecular_Direct * max(0.01, _specularIntensity), max(0.001, _waterRoughness), finalWorldNormal, cameraPosition, light_Dir, light_Color, 1);
                float3 water_IndirectLighting = WaterIndirectLightingBRDF(waterDiffuse_Direct, waterSpecular_Direct, max(0.001, _waterRoughness), worldPosition, finalWorldNormal, cameraPosition,
                screenPosition.xy, _reflectionDisort, depth_water * max(0.0, _reflectionIntensity), _PlanarReflectionTexture, sampler_PlanarReflectionTexture, 1);

                ////////////////////////
                //    Final Output    //
                ////////////////////////
                float3 finalColor = water_DirectLighting + water_IndirectLighting;
                return float4(finalColor, depth_water);
            }
            ENDHLSL
        }
    }
    CustomEditor "LWGUI.LWGUI"
}