with Bounding_Boxes;

with GL.Types;

generic
   use Bounding_Boxes;
   use GL.Types;

   type Primitive_Type is private;
   type Primitive_Array is array (Positive range <>) of Primitive_Type;

   with function Get_Centroid
     (P : Primitive_Type) return Singles.Vector3;

   with function Get_Bounding_Box
     (P : Primitive_Type) return Bounding_Box;

   Max_Volume_Elements : Natural := 5;
package Bounding_Volume_Hierarchies is

   type BVH (<>) is private;

   function Compute_BVH (Primitives : Primitive_Array) return BVH;

   procedure Dump (Root : BVH);

   type Index_Array is array (Positive range <>) of Positive;

   generic
      type T is private;
   package Visitors is
      type Leaf_Visitor is access function
        (BB      : Bounding_Box;
         Indices : Index_Array) return T;

      type Node_Visitor is access function
        (BB    : Bounding_Box;
         Axis  : GL.Index_3D;
         Left  : BVH;
         Right : BVH) return T;

      function Visit
        (Root       : BVH;
         Visit_Leaf : Leaf_Visitor;
         Visit_Node : Node_Visitor) return T;
   end Visitors;

private
   type Partition_Range is record
      First, Last : Positive;
   end record;

   type BVH_Node;

   type BVH_Node_Access is access BVH_Node;

   type BVH_Node (Is_Leaf : Boolean) is record
      BB    : Bounding_Box;
      Rng   : Partition_Range;
      case Is_Leaf is
         when True =>
            null;
         when False =>
            Axis  : GL.Index_3D;
            Left  : BVH_Node_Access := null;
            Right : BVH_Node_Access := null;
      end case;
   end record;

   type Index_Array_Access is access all Index_Array;

   type Partition_Array is array (GL.Index_3D) of Index_Array_Access;

   type BVH is record
      Root       : BVH_Node_Access;
      Partitions : Partition_Array;
   end record;
end Bounding_Volume_Hierarchies;
