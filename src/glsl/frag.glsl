#version 120
#define SPHERE 0
#define PLANE 1
#define CUBE 2

const float epsilon = 0.001f;
const float min_step_size = 0.005f;
const float max_dist = 20;
const int max_steps = 300;
const float PI = 3.1415926535897932;

varying vec4 pos;

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

uniform float time;
uniform vec3 camera_position;

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
   float denominator = 4.0 * max(dot(N, V), 0.0) * max(dot(N, L), 0.0);
   vec3 specular     = numerator / max(denominator, 0.001);

   // shadow cast

   float shadows = softshadows(
      pos + normal * min_step_size * 5,
      L,
      min_step_size,
      light_distance,
      64
   );

   // add to outgoing radiance Lo
   float NdotL = max(dot(N, L), 0.0);
   Lo += (kD * albedo / PI + specular) * radiance * NdotL * mix(0.1, 1, shadows);

   // finally
   vec3 ambient = vec3(0.03, 0.04, 0.1) * albedo * 0.1;
   Lo += ambient;

   return Lo + ambient;
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

void main(void)
{
   vec3 frag_pos = vec3(pos.xy, 0);
   vec3 dir = normalize(frag_pos - vec3(0, 0, -1.5));
   vec3 light_pos = vec3(cos(time), 2, sin(time));
   vec3 initial_pos = frag_pos + camera_position;

   vec3 col = pixel_color_1(initial_pos, dir, light_pos);
   gl_FragColor = vec4(pow(col / (col + vec3(1.0)), vec3(0.4545)), 1);
}
