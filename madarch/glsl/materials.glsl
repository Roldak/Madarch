struct Material {
   vec3 albedo;
   float metallic;
   float roughness;
};

layout(std140, binding = 2) uniform materials_description {
   int material_count;
   Material materials[20];
};

