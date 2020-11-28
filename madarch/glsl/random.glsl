float hash(float seed) {
    return fract(sin(seed) * 43758.5453);
}

highp float rand(vec2 co) {
    highp float a  = 12.9898;
    highp float b  = 78.233;
    highp float c  = 43758.5453;
    highp float dt = dot(co.xy, vec2(a,b));
    highp float sn = mod(dt, PI);
    return fract(sin(sn) * c);
}

vec3 random_sphere_point(float seed) {
   float ang1 = hash(78.233 + seed) * PI_2; // [0..1) -> [0..2*PI)
   float u = hash(10.873 + seed) * 2 - 1; // [0..1) -> [-1..1)
   float u2 = u * u;
   float sqrt1MinusU2 = sqrt(1.0 - u2);
   float x = sqrt1MinusU2 * cos(ang1);
   float y = sqrt1MinusU2 * sin(ang1);
   float z = u;
   return vec3(x, y, z);
}

vec3 random_hemisphere_point(float seed, vec3 n) {
   vec3 v = random_sphere_point(seed);
   return v * sign(dot(v, n));
}

vec3 cosine_direction(in float seed, in vec3 nor) {
    // compute basis from normal
    vec3 tc = vec3(1.0 + nor.z - nor.xy * nor.xy, -nor.x * nor.y) / (1.0 + nor.z);
    vec3 uu = vec3(tc.x, tc.z, -nor.x);
    vec3 vv = vec3(tc.z, tc.y, -nor.y);

    float u = hash(78.233 + seed);
    float v = hash(10.873 + seed);
    float a = PI_2 * v;

    return  sqrt(u) * (cos(a) * uu + sin(a) * vv) + sqrt(1.0 - u) * nor;
}

vec3 uniform_vector(in float seed) {
    float a = PI   * hash(78.233 + seed);
    float b = PI_2 * hash(10.873 + seed);
    return vec3(sin(b) * sin(a), cos(b) * sin(a), cos(a));
}

