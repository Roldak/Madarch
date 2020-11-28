vec3 fresnel_schlick(float cosTheta, vec3 F0) {
    return F0 + (1.0 - F0) * pow(1.001 - cosTheta, 5.0);
}

float distribution_GGX(vec3 N, vec3 H, float roughness) {
    float a      = roughness*roughness;
    float a2     = a*a;
    float NdotH  = max(dot(N, H), 0.0);
    float NdotH2 = NdotH*NdotH;

    float num   = a2;
    float denom = (NdotH2 * (a2 - 1.0) + 1.0);
    denom = PI * denom * denom;

    return num / denom;
}

float geometry_schlick_GGX(float NdotV, float roughness) {
    float r = (roughness + 1.0);
    float k = (r*r) / 8.0;

    float num   = NdotV;
    float denom = NdotV * (1.0 - k) + k;

    return num / denom;
}

float geometry_smith(float NdotV, float NdotL, float roughness) {
    float ggx2  = geometry_schlick_GGX(NdotV, roughness);
    float ggx1  = geometry_schlick_GGX(NdotL, roughness);

    return ggx1 * ggx2;
}

void cook_torrance_coefficients (vec3 N, vec3 V, vec3 L, float NdotL,
                                 vec3 albedo, float metallic, float roughness,
                                 out vec3 kD, out vec3 kS) {
   vec3 H = normalize(V + L);
   float NdotV = max(dot(N, V), 0.0);

   // cook-torrance BRDF
   vec3 F0 = mix(vec3(0.04), albedo, metallic);
   float NDF = distribution_GGX(N, H, roughness);
   float G   = geometry_smith(NdotV, NdotL, roughness);
   vec3  F   = fresnel_schlick(max(dot(H, V), 0.0), F0);

   vec3 numerator    = NDF * G * F;
   float denominator = 4.0 * NdotV * NdotL;

   kD = (vec3(1.0) - F) * (1.0 - metallic);
   kS = min(numerator / max(denominator, 0.001), 1.0);
}

