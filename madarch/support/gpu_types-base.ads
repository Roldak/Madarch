package GPU_Types.Base is
   Int    : constant GPU_Type;
   IVec_2 : constant GPU_Type;
   IVec_3 : constant GPU_Type;

   Float  : constant GPU_Type;
   Vec_2  : constant GPU_Type;
   Vec_3  : constant GPU_Type;
private
   type Base is new GPU_Type_Internal with record
      Fixed_Alignment : Types.Size;
      Fixed_Size      : Types.Size;
   end record;

   overriding function Alignment (X : Base) return Types.Size is
     (X.Fixed_Alignment);

   overriding function Size (X : Base) return Types.Size is
     (X.Fixed_Size);

   R_Int : aliased Base :=
     (GPU_Type_Internal with Fixed_Alignment => 4, Fixed_Size => 4);

   R_IVec_2 : aliased Base :=
     (GPU_Type_Internal with Fixed_Alignment => 8, Fixed_Size => 8);

   R_IVec_3 : aliased Base :=
     (GPU_Type_Internal with Fixed_Alignment => 16, Fixed_Size => 12);

   R_Float : aliased Base :=
     (GPU_Type_Internal with Fixed_Alignment => 4, Fixed_Size => 4);

   R_Vec_2 : aliased Base :=
     (GPU_Type_Internal with Fixed_Alignment => 8, Fixed_Size => 8);

   R_Vec_3 : aliased Base :=
     (GPU_Type_Internal with Fixed_Alignment => 16, Fixed_Size => 12);

   Int    : constant GPU_Type := (T => R_Int'Access);
   IVec_2 : constant GPU_Type := (T => R_IVec_2'Access);
   IVec_3 : constant GPU_Type := (T => R_IVec_3'Access);

   Float  : constant GPU_Type := (T => R_Float'Access);
   Vec_2  : constant GPU_Type := (T => R_Vec_2'Access);
   Vec_3  : constant GPU_Type := (T => R_Vec_3'Access);
end GPU_Types.Base;
