package GPU_Types.Base is
   type Base is new GPU_Type with record
      Fixed_Alignment : Types.Size;
      Fixed_Size      : Types.Size;
   end record;

   overriding function Alignment (X : Base) return Types.Size is
     (X.Fixed_Alignment);

   overriding function Size (X : Base) return Types.Size is
     (X.Fixed_Size);

   Int    : constant GPU_Type_Access;
   IVec_2 : constant GPU_Type_Access;
   IVec_3 : constant GPU_Type_Access;

   Float  : constant GPU_Type_Access;
   Vec_2  : constant GPU_Type_Access;
   Vec_3  : constant GPU_Type_Access;
private
   R_Int : aliased constant Base :=
     (GPU_Type with Fixed_Alignment => 4, Fixed_Size => 4);

   R_IVec_2 : aliased constant Base :=
     (GPU_Type with Fixed_Alignment => 8, Fixed_Size => 8);

   R_IVec_3 : aliased constant Base :=
     (GPU_Type with Fixed_Alignment => 16, Fixed_Size => 12);

   R_Float : aliased constant Base :=
     (GPU_Type with Fixed_Alignment => 4, Fixed_Size => 4);

   R_Vec_2 : aliased constant Base :=
     (GPU_Type with Fixed_Alignment => 8, Fixed_Size => 8);

   R_Vec_3 : aliased constant Base :=
     (GPU_Type with Fixed_Alignment => 16, Fixed_Size => 12);

   Int    : constant GPU_Type_Access := R_Int'Access;
   IVec_2 : constant GPU_Type_Access := R_IVec_2'Access;
   IVec_3 : constant GPU_Type_Access := R_IVec_3'Access;

   Float : constant GPU_Type_Access := R_Float'Access;
   Vec_2 : constant GPU_Type_Access := R_Vec_2'Access;
   Vec_3 : constant GPU_Type_Access := R_Vec_3'Access;
end GPU_Types.Base;
