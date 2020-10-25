#version 120
#define SPHERE 0
#define PLANE 1
#define CUBE 2

const float epsilon = 0.001f;
const float min_step_size = 0.005f;
const float max_dist = 20;
const int max_steps = 300;
const float PI   = 3.14159265358;
const float PI_2 = 6.28318530718;

const int gi_bounces = 3;
const int gi_samples = 1;

/****************
 * Random utils *
 ****************/

float hash(float seed) {
    return fract(sin(seed) * 43758.5453);
}

highp float rand(vec2 co)
{
    highp float a  = 12.9898;
    highp float b  = 78.233;
    highp float c  = 43758.5453;
    highp float dt = dot(co.xy, vec2(a,b));
    highp float sn = mod(dt, PI);
    return fract(sin(sn) * c);
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

/******************************
 * Cook-Terrance BRDF helpers *
 ******************************/

vec3 fresnelSchlick(float cosTheta, vec3 F0) {
    return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0);
}

float DistributionGGX(vec3 N, vec3 H, float roughness)
{
    float a      = roughness*roughness;
    float a2     = a*a;
    float NdotH  = max(dot(N, H), 0.0);
    float NdotH2 = NdotH*NdotH;

    float num   = a2;
    float denom = (NdotH2 * (a2 - 1.0) + 1.0);
    denom = PI * denom * denom;

    return num / denom;
}

float GeometrySchlickGGX(float NdotV, float roughness)
{
    float r = (roughness + 1.0);
    float k = (r*r) / 8.0;

    float num   = NdotV;
    float denom = NdotV * (1.0 - k) + k;

    return num / denom;
}
float GeometrySmith(vec3 N, vec3 V, vec3 L, float roughness)
{
    float NdotV = max(dot(N, V), 0.0);
    float NdotL = max(dot(N, L), 0.0);
    float ggx2  = GeometrySchlickGGX(NdotV, roughness);
    float ggx1  = GeometrySchlickGGX(NdotL, roughness);

    return ggx1 * ggx2;
}

/**************************
 * Signed distance fields *
 **************************/

//sphere

float dist_to_sphere(vec3 x, vec3 center, float radius) {
   return length(center - x) - radius;
}

vec3 sphere_normal(vec3 x, vec3 center, float radius) {
   return normalize(x - center);
}

//plane

float dist_to_plane(vec3 x, vec3 normal, float height) {
   return dot(x, normal) + height;
}

vec3 plane_normal(vec3 x, vec3 normal, float height) {
   return normal;
}

//cube

float dist_to_cube(vec3 x, vec3 center, float size) {
   vec3 q = abs(center - x) - vec3(size);
   return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

vec3 cube_normal(vec3 x, vec3 center, float size) {
   vec3 d = x - center;
   float rx = abs(d.x);
   float ry = abs(d.y);
   float rz = abs(d.z);
   return normalize(vec3(
      float(rx >= ry - epsilon) * float(rx >= rz - epsilon) * sign(d.x),
      float(ry >= rx - epsilon) * float(ry >= rz - epsilon) * sign(d.y),
      float(rz >= rx - epsilon) * float(rz >= ry - epsilon) * sign(d.z)
   ));
}

struct Primitive {
   int kind;
   vec3 vec3_param_1;
   float float_param_1;

   vec3 color;
   float mirror;
   float metallic;
   float roughness;
};

float dist_to_primitive(vec3 x, Primitive prim) {
   switch (prim.kind) {
      case SPHERE:
         return dist_to_sphere(x, prim.vec3_param_1, prim.float_param_1);
      case PLANE:
         return dist_to_plane(x, prim.vec3_param_1, prim.float_param_1);
      case CUBE:
         return dist_to_cube(x, prim.vec3_param_1, prim.float_param_1);
   }
}

vec3 primitive_normal(vec3 x, Primitive prim) {
   switch (prim.kind) {
      case SPHERE:
         return sphere_normal(x, prim.vec3_param_1, prim.float_param_1);
      case PLANE:
         return plane_normal(x, prim.vec3_param_1, prim.float_param_1);
      case CUBE:
         return cube_normal(x, prim.vec3_param_1, prim.float_param_1);
   }
}

/**********************
 * Program definition *
 **********************/

varying vec4 pos;
uniform float seed;
uniform float time;
uniform vec3 camera_position;

#define PRIM_COUNT 8
const Primitive prims[PRIM_COUNT] = Primitive[](
   Primitive(SPHERE, vec3(0.5, 0.5, 1.0), 1.0, vec3(1, 0, 0), 0, 0.1, 0.1),
   Primitive(CUBE,   vec3(-0.5, 0, 2.0),  0.8, vec3(0, 0, 1), 0, 0.8, 0.8),

   Primitive(PLANE, vec3(0, 1, 0),  1.0, vec3(0.5, 0.5, 0.5), 0, 0, 0.6),
   Primitive(PLANE, vec3(0, -1, 0), 5.0, vec3(0.5, 0.5, 0.5), 0, 0, 0.6),
   Primitive(PLANE, vec3(1, 0, 0),  5.0, vec3(0.5, 0.5, 0.5), 0, 0, 0.6),
   Primitive(PLANE, vec3(-1, 0, 0), 5.0, vec3(0.5, 0.5, 0.5), 0, 0, 0.6),
   Primitive(PLANE, vec3(0, 0, 1),  5.0, vec3(0.5, 0.5, 0.5), 0, 0, 0.6),
   Primitive(PLANE, vec3(0, 0, -1), 5.0, vec3(0.5, 0.5, 0.5), 0, 0, 0.6)
);

float closest_primitive(vec3 x, out int index) {
   float closest = max_dist;
   for (int i = 0; i < PRIM_COUNT; ++i) {
      float dist = dist_to_primitive (x, prims[i]);
      if (dist < closest) {
         closest = dist;
         index = i;
      }
   }
   return closest;
}

float softshadows(vec3 from, vec3 dir, float min_dist, float max_dist, float k) {
   int index;

   float res = 1.0;
   float prev_dist = 1e20;

   for (float total_dist = min_dist; total_dist < max_dist;) {
      float dist = closest_primitive(from + dir * total_dist, index);

      if (dist < epsilon) {
         return 0.0;
      }

      float y = dist * dist / (2.0 * prev_dist);
      float d = sqrt(dist * dist - y * y);
      res = min(res, k * d / max(0.0, total_dist - y));

      prev_dist = dist;
      total_dist += dist;
   }
   return res;
}

bool raycast(vec3 from, vec3 dir, out int index, out vec3 coll) {
   for (float total_dist = 0; total_dist < max_dist;) {
      float dist = closest_primitive(from, index);

      if (dist < epsilon) {
         coll = from + dir * dist;
         return true;
      }

      from += dir * dist;
      total_dist += dist;
   }
   return false;
}

vec3 shade(vec3 pos, vec3 normal, vec3 dir, vec3 light_pos,
           vec3 albedo, float metallic, float roughness) {

   // reflectance
   vec3 N = normal;
   vec3 V = -dir;

   vec3 F0 = mix(vec3(0.04), albedo, metallic);

   vec3 Lo = vec3(0.0);

   // do for each light:
   vec3 light_dir = light_pos - pos;
   float light_distance = length(light_dir);
   vec3 L = light_dir / light_distance;
   vec3 H = normalize(V + L);
   float NdotL = max(dot(N, L), 0.0);

   // calculate light radiance
   float attenuation = 1.0; // / (light_distance * light_distance);
   vec3 radiance     = vec3(0.9, 0.9, 0.8) * attenuation;

   // cook-torrance BRDF
   float NDF = DistributionGGX(N, H, roughness);
   float G   = GeometrySmith(N, V, L, roughness);
   vec3  F   = fresnelSchlick(max(dot(H, V), 0.0), F0);

   vec3 kS = F;
   vec3 kD = vec3(1.0) - kS;
   kD *= 1.0 - metallic;

   vec3 numerator    = NDF * G * F;
   float denominator = 4.0 * max(dot(N, V), 0.0) * NdotL;
   vec3 specular     = numerator / max(denominator, 0.001);

   // shadow cast
   float shadows = 0.0;
   if (NdotL > epsilon) {
      shadows = softshadows(
         pos + normal * min_step_size * 5,
         L,
         min_step_size,
         light_distance,
         64
      );
   }

   // add to outgoing radiance Lo
   Lo += (kD * albedo / PI + specular) * radiance * NdotL * mix(0.1, 1, shadows);

   // compute ambient
   // vec3 ambient = vec3(0.03, 0.04, 0.1) * albedo * 0.2;
   // Lo += ambient;

   return Lo;
}

vec3 fog(vec3 from, vec3 pos, vec3 col, vec3 bg) {
   float dist = length(pos - from);
   float offset = dist - 0.8 * max_dist;
   return mix(col, bg, max(offset / (0.2 * max_dist), 0));
}

vec3 pixel_color_0(vec3 from, vec3 dir, vec3 light_pos) {
   vec3 background_color = vec3(0.30, 0.36, 0.60) - (dir.y * 0.7);

   int prim_index;
   vec3 pos;
   if (raycast(from, dir, prim_index, pos)) {
      vec3 albedo = prims[prim_index].color;
      float metallic = prims[prim_index].metallic;
      float roughness = prims[prim_index].roughness;
      vec3 normal = primitive_normal(pos, prims[prim_index]);
      vec3 result = shade(pos, normal, dir, light_pos, albedo, metallic, roughness);
      return fog(from, pos, result, background_color);

   }
   return background_color;
}

vec3 pixel_color_1(vec3 from, vec3 dir, vec3 light_pos) {
   vec3 background_color = vec3(0.30, 0.36, 0.60) - (dir.y * 0.7);

   int prim_index;
   vec3 pos;
   if (raycast(from, dir, prim_index, pos)) {
      vec3 albedo = prims[prim_index].color;
      float metallic = prims[prim_index].metallic;
      float roughness = prims[prim_index].roughness;
      vec3 normal = primitive_normal(pos, prims[prim_index]);
      vec3 result = shade(pos, normal, dir, light_pos, albedo, metallic, roughness);

      // reflection

      float mirror = prims[prim_index].mirror;
      if (mirror > 0) {
         result = mix(
            result,
            pixel_color_0(
               pos + normal * min_step_size * 5,
               reflect(dir, normal),
               light_pos
            ),
            mirror
         );
      }

      return fog(from, pos, result, background_color);
   }
   return background_color;
}

vec3 pixel_color_2(vec3 from, vec3 dir, vec3 light_pos) {
   vec3 background_color = vec3(0.30, 0.36, 0.60) - (dir.y * 0.7);

   int prim_index;
   vec3 pos;
   if (raycast(from, dir, prim_index, pos)) {
      vec3 albedo = prims[prim_index].color;
      float metallic = prims[prim_index].metallic;
      float roughness = prims[prim_index].roughness;
      vec3 normal = primitive_normal(pos, prims[prim_index]);
      vec3 result = shade(pos, normal, dir, light_pos, albedo, metallic, roughness);

      // reflection

      float mirror = prims[prim_index].mirror;
      if (mirror > 0) {
         result = mix(
            result,
            pixel_color_1(
               pos + normal * min_step_size * 5,
               reflect(dir, normal),
               light_pos
            ),
            mirror
         );
      }

      return fog(from, pos, result, background_color);
   }
   return background_color;
}

vec3 pixel_color_path(vec3 from, vec3 dir, vec3 light_pos, float sa) {
   vec3 background_color = vec3(0.30, 0.36, 0.60) - (dir.y * 0.7);
   vec3 result = vec3(0.0);
   vec3 mask = vec3(1.0);

   for (int bounce = 0; bounce < gi_bounces + 1; ++bounce) {
      int prim_index;
      vec3 pos;
      if (raycast(from, dir, prim_index, pos)) {
         vec3 albedo = prims[prim_index].color;
         float metallic = prims[prim_index].metallic;
         float roughness = prims[prim_index].roughness;
         vec3 normal = primitive_normal(pos, prims[prim_index]);

         result += mask * shade(pos, normal, dir, light_pos, albedo, metallic, roughness);
         mask *= albedo;

         from = pos;

         // BRDF
         if (rand(vec2(sa + time * 127.2, sa + 7.7 * float(bounce))) < roughness) {
            dir = cosine_direction(sa + 76.2 + 73.1 * float(bounce) + 17.7 * time, normal);
         } else {
            vec3 reflected = reflect(dir, normal);
            vec3 offset = uniform_vector(sa + time * 111.123 + 65.2 * float(bounce));
            dir = normalize(reflected + offset * (1 - roughness));
         }
      } else {
         return background_color;
      }
   }
   return result;
}

void main(void)
{
   vec3 frag_pos = vec3(pos.xy, 0);
   vec3 dir = normalize(frag_pos - vec3(0, 0, -1.5));
   vec3 light_pos = vec3(cos(time), 2, sin(time));
   vec3 initial_pos = frag_pos + camera_position;

   float sa = rand(pos.xy * 1113.1 * time);

   vec3 col = pixel_color_path(initial_pos, dir, light_pos, sa);
   gl_FragColor = vec4(pow(col / (col + vec3(1.0)), vec3(0.4545)), 1);
}
