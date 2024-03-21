#include "./WaterLighting.hlsl"

///////////////////////////////
///                         ///
///       ToneMapping       ///
///                         ///
///////////////////////////////
float3 ACES_Tonemapping(float3 color_input)
{
    float a = 2.51f;
    float b = 0.03f;
    float c = 2.43f;
    float d = 0.59f;
    float e = 0.14f;
    float3 encode_color = saturate((color_input * (a * color_input + b)) / (color_input * (c * color_input + d) + e));
    return encode_color;
}

///////////////////////
//    Water alpha    //
///////////////////////
float GetWaterDepth(Texture2D CameraDepthTex, sampler sampler_CameraDepthTex, float2 screenPosXY, float viewPos_worldZ, float shallowRange, float Opacity)
{
    float depthTexture = SAMPLE_TEXTURE2D(CameraDepthTex, sampler_CameraDepthTex, screenPosXY).x;
    float depthScene = LinearEyeDepth(depthTexture, _ZBufferParams);
    float depthWater = depthScene + viewPos_worldZ;
    depthWater = (1.0 - saturate(exp( - (depthWater) / max(0.5, shallowRange)))) * Opacity;

    return depthWater;
}

////////////////////////////////////////////////
///                                          ///
///    ReconstructWorldPositionfromDepth     ///
///                                          ///
////////////////////////////////////////////////
float4 ReconstructWorldPosition(Texture2D CameraDepthTex, sampler sampler_CameraDepthTex, float2 screenPosXY, float3 viewPos_world)
{
    float depthTexture = SAMPLE_TEXTURE2D(CameraDepthTex, sampler_CameraDepthTex, screenPosXY).x;

    float depth = LinearEyeDepth(depthTexture, _ZBufferParams);
    // Restructe world position from depth
    float4 depthVS = float4(1.0, 1.0, 1.0, 1.0);
    depthVS.xy = viewPos_world.xy * depth / - viewPos_world.z;
    depthVS.z = depth;
    
    return mul(unity_CameraToWorld, depthVS);
}

////////////////////////////
///                      ///
///    FresnelFactor     ///
///                      ///
////////////////////////////
float GetFresnelFactor(float3 normal, float3 viewDir, float range, float intensity)
{
    float factor = saturate(dot(normal, viewDir));

    return saturate(pow(factor, range) * intensity);
}

/////////////////////////
//    Water Specular   //
/////////////////////////
float3 GetWaterSpecularColor(float3 specular, float3 normal, float radius, float3 lightDir, float3 viewDir, float3 lightCol)
{
    float a2 = Pow4_UE4(pow(saturate(normal.z * 0.5 + 0.5), radius * 2.0));
    float3 H = normalize(lightDir + viewDir);
    float NoH = saturate(dot(normal, H));
    float NoV = saturate(abs(dot(normal, viewDir)) + 1e-5);
    float VoH = saturate(dot(viewDir, H));
    float NoL = saturate(dot(normal, lightDir));
    float D = D_GGX_UE4(a2, NoH);
    float Vis = Vis_SmithJointApprox(a2, NoV, NoL);
    float3 F = F_Schlick_UE4(specular, VoH);
    float3 radians = NoL * lightCol;
    
    return (D * Vis) * F * radians;
}

///////////////////////////
///                     ///
///      Caustics       ///
///                     ///
///////////////////////////
float3 GetCausticsColor(float3 Color, float Mask, Texture2D causticsTex, sampler causticsSampler, float2 UV, vector causticsParams, float2 distortDir, float distortStrength)
{
    // small tilling(XY) intensity(Z) speed(W)
    float2 uv_Caustics = UV * causticsParams.xy + distortDir * distortStrength;
    float speed_Caustics = causticsParams.w * _Time.y;

    float3 caustics_A = SAMPLE_TEXTURE2D(causticsTex, causticsSampler, uv_Caustics + speed_Caustics * float2(0.6157, 1.3268)).xyz;
    float3 caustics_B = SAMPLE_TEXTURE2D(causticsTex, causticsSampler, -uv_Caustics + speed_Caustics * float2(0.8335, 1.6724)).xyz;

    return min(caustics_A, caustics_B) * max(0.0, causticsParams.z) * Color * Mask;
}

//////////////////////
//    Foam Color    //
//////////////////////
// float3 GetFoamColor(float3 color ,Texture2D foamTex, sampler foamSampler, vector foamParams, float2 distortDir, float distortStrength)
// {
//     float2 speed_Foam = _Time.y * foamParams.w;
//     float foamMask = SAMPLE_TEXTURE2D(foamTex, foamSampler, foamParams.xy * worldPosition.xz + speed_Foam).x;
//     foamMask = saturate(pow(foamMask, 8));
//     foamMask *= saturate(pow(wavePosition.y, 8) - 0.3);
//     float3 foamColor = foamMask * color;

//     return foamColor;
// }

///////////////////////////
///                     ///
///     Refraction      ///
///                     ///
///////////////////////////
float3 GetRefractionColor(float2 distortUV, Texture2D CameraDepthTex, sampler sampler_CameraDepthTex, float positionVSz, Texture2D CameraOpaqueTex, sampler sampler_CameraOpaqueTex, float2 screenPosXY)
{
    // ���²���ԭʼ���ͼ������ˮ�����ֵ���Ŷ���Ľ��
    float depthDistortTexture = SAMPLE_TEXTURE2D(CameraDepthTex, sampler_CameraDepthTex, distortUV).x;
    float depthDistortScene = LinearEyeDepth(depthDistortTexture, _ZBufferParams);
    // ˮ��Ť��������ֵ����Ч�������룬���ñ�Ť���ĵط�Ҳ��Ť���ˣ�
    float depthDistortWater = depthDistortScene + positionVSz;
    // �޸�������ͨ���Ƚ����ֵ�� 0 �Ĺ�ϵ���ж���Ҫ��Ť��������
    // ��С�� 0 ʱ��ʹ��δ��Ť�������ֵ�������ڵ��� 0 ʱʹ�ñ�Ť�������ֵ
    // if (depthDistortWater < 0)depthDistortWater = depthWater;
    if (depthDistortWater < 0)
        distortUV = screenPosXY;

    return SAMPLE_TEXTURE2D(CameraOpaqueTex, sampler_CameraOpaqueTex, distortUV).xyz;
}