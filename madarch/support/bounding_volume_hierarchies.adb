with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Generic_Array_Sort;

package body Bounding_Volume_Hierarchies is
   use GL;

   package Primitive_Vectors is new Ada.Containers.Vectors
     (Positive, Primitive_Type);

   generic
      Prims : Primitive_Array;
      Axis  : GL.Index_3D;
   function Centroid_Comparer (I, J : Positive) return Boolean;

   function Centroid_Comparer (I, J : Positive) return Boolean is
     (Get_Centroid (Prims (I)) (Axis) < Get_Centroid (Prims (J)) (Axis));

   type BB_Cost is record
      BB   : Bounding_Box;
      Cost : Single;
   end record;

   type BB_Cost_Array is array (Positive range <>) of BB_Cost;

   type Boolean_Array is array (Positive range <>) of Boolean;

   procedure Partition_According_To
     (Output : in out Index_Array;
      Flags  : Boolean_Array)
   is
      Input  : Index_Array (Output'Range) := Output;
      Cursor : Positive := Output'First;
   begin
      for I in Input'Range loop
         if Flags (Input (I)) then
            Output (Cursor) := Input (I);
            Cursor := Cursor + 1;
         end if;
      end loop;
      for I in Input'Range loop
         if not Flags (Input (I)) then
            Output (Cursor) := Input (I);
            Cursor := Cursor + 1;
         end if;
      end loop;
   end Partition_According_To;

   procedure Print_Range (Rng : Partition_Range) is
   begin
      Put ("Range :");
      Put (Rng.First'Image);
      Put (" ..");
      Put_Line (Rng.Last'Image);
   end Print_Range;

   function Process_Node
     (Parent_BB  : Bounding_Box;
      Primitives : Primitive_Array;
      Partitions : Partition_Array;
      Rng        : Partition_Range;
      Depth      : Natural) return BVH_Node_Access
   is
      First      : constant Positive := Rng.First;
      Last       : constant Positive := Rng.Last;
      Count      : constant Natural := Last - First + 1;
      Parent_SA  : constant Single := Surface_Area (Parent_BB);

      Best_Axis  : Index_3D := X;
      Best_Cost  : Single   := 0.0;
      Best_Index : Natural  := 0;

      Best_BB_Left, Best_BB_Right : Bounding_Box;
   begin
      if Count <= Max_Volume_Elements then
         return new BVH_Node'
           (Is_Leaf => True,
            BB      => Parent_BB,
            Rng     => Rng);
      end if;

      --  Find axis to split
      for Axis in Index_3D loop
         declare
            Indices    : constant Index_Array_Access := Partitions (Axis);

            Right_BB   : Bounding_Box :=
               Get_Bounding_Box (Primitives (Indices (Last)));

            Left_BB    : Bounding_Box :=
               Get_Bounding_Box (Primitives (Indices (First)));

            Right_BBs  : BB_Cost_Array (First .. Last);

            Side_Cost  : Single;
            Full_Cost  : Single;
         begin
            --  First traversal right-to-left to store right SAs
            for I in reverse First + 1 .. Last loop
               Extend (Right_BB, Get_Bounding_Box (Primitives (Indices (I))));
               Side_Cost :=
                  Surface_Area (Right_BB) / Parent_SA * Single (Last - I + 1);
               Right_BBs (I) := BB_Cost'(Right_BB, Side_Cost);
            end loop;

            --  Now traverse left-to-right to compute full SAH and retrieve
            --  index for which it is the smallest.
            for I in First .. Last - 1 loop
               Extend (Left_BB, Get_Bounding_Box (Primitives (Indices (I))));
               Side_Cost :=
                  Surface_Area (Left_BB) / Parent_SA * Single (I - First + 1);
               Full_Cost := Side_Cost + Right_BBs (I + 1).Cost;

               if Best_Index = 0 or else Full_Cost < Best_Cost then
                  Best_Cost     := Full_Cost;
                  Best_Index    := I;
                  Best_Axis     := Axis;
                  Best_BB_Left  := Left_BB;
                  Best_BB_Right := Right_BBs (I + 1).BB;
               end if;
            end loop;
         end;
      end loop;

      --  Best_Index must have been set by now
      pragma Assert (Best_Index /= 0);
      pragma Assert (Contains (Parent_BB, Best_BB_Left));
      pragma Assert (Contains (Parent_BB, Best_BB_Right));

      declare
         Left_Count  : constant Natural := Best_Index - First;

         Membership  : Boolean_Array (Primitives'Range);

         Left_Range  : Partition_Range := (First, First + Left_Count);
         Right_Range : Partition_Range := (First + Left_Count + 1, Last);
      begin
         for I in First .. Last loop
            Membership (Partitions (Best_Axis).all (I)) := I <= Best_Index;
         end loop;

         for Axis in Index_3D loop
            if Axis not in Best_Axis then
               Partition_According_To
                 (Partitions (Axis).all (First .. Last),
                  Membership);
            end if;
         end loop;

         return new BVH_Node'
           (Is_Leaf => False,
            BB      => Parent_BB,
            Rng     => Rng,
            Axis    => Best_Axis,
            Left    => Process_Node
              (Best_BB_Left,  Primitives, Partitions, Left_Range, Depth + 1),
            Right   => Process_Node
              (Best_BB_Right, Primitives, Partitions, Right_Range, Depth + 1));
      end;
   end Process_Node;

   function Compute_BVH (Primitives : Primitive_Array) return BVH is
      function Centroid_X_Comparer is new Centroid_Comparer (Primitives, X);
      function Centroid_Y_Comparer is new Centroid_Comparer (Primitives, Y);
      function Centroid_Z_Comparer is new Centroid_Comparer (Primitives, Z);

      procedure Sort_By_Centroid_X is new Ada.Containers.Generic_Array_Sort
        (Positive, Positive, Index_Array, Centroid_X_Comparer);

      procedure Sort_By_Centroid_Y is new Ada.Containers.Generic_Array_Sort
        (Positive, Positive, Index_Array, Centroid_Y_Comparer);

      procedure Sort_By_Centroid_Z is new Ada.Containers.Generic_Array_Sort
        (Positive, Positive, Index_Array, Centroid_Z_Comparer);

      Sort_Function : array (Index_3D) of
         access procedure (Arr : in out Index_Array) :=
        (Sort_By_Centroid_X'Access,
         Sort_By_Centroid_Y'Access,
         Sort_By_Centroid_Z'Access);

      Partitions : Partition_Array :=
        (X => new Index_Array (Primitives'Range),
         Y => new Index_Array (Primitives'Range),
         Z => new Index_Array (Primitives'Range));

      Rng : Partition_Range := (Primitives'First, Primitives'Last);

      Root_BB : Bounding_Box;
   begin
      if Primitives'Length = 0 then
         return (null, Partitions);
      end if;

      Root_BB := Get_Bounding_Box (Primitives (1));

      for I in Primitives'Range loop
         for C in Index_3D loop
            Partitions (C) (I) := I;
         end loop;
         Extend (Root_BB, Get_Bounding_Box (Primitives (I)));
      end loop;

      for C in Index_3D loop
         Sort_Function (C).all (Partitions (C).all);
      end loop;

      return
        (Root       => Process_Node (Root_BB, Primitives, Partitions, Rng, 1),
         Partitions => Partitions);
   end Compute_BVH;

   package body Visitors is
      function Visit
        (Root       : BVH;
         Visit_Leaf : Leaf_Visitor;
         Visit_Node : Node_Visitor) return T
      is
         Root_Node : BVH_Node_Access := Root.Root;
      begin
         case Root_Node.Is_Leaf is
            when True =>
               declare
                  First : constant Positive := Root_Node.Rng.First;
                  Last  : constant Positive := Root_Node.Rng.First;
                  Count : constant Positive := Last - First + 1;
                  Indices : Index_Array (1 .. Count);
               begin
                  for I in Indices'Range loop
                     Indices (I) := Root.Partitions (X) (I + First - 1);
                  end loop;
                  return Visit_Leaf (Root_Node.BB, Indices);
               end;
            when False =>
               return Visit_Node
                 (Root_Node.BB,
                  Root_Node.Axis,
                  (Root => Root_Node.Left,  Partitions => Root.Partitions),
                  (Root => Root_Node.Right, Partitions => Root.Partitions));
         end case;
      end Visit;
   end Visitors;

   type Empty_Type is null record;

   package Void_Visitors is new Visitors (Empty_Type);

   procedure Dump (Root : BVH) is
      Indent : Natural := 0;

      function Visit_BVH_Leaf
        (BB      : Bounding_Box;
         Indices : Index_Array) return Empty_Type
      is
         Prefix : String := (1 .. Indent => ' ');
      begin
         Put_Line (Prefix & "Leaf");
         Put_Line (Prefix & "  BB : " & Image (BB));
         Put (Prefix & "  Indices :");
         for I of Indices loop
            Put (I'Image);
         end loop;
         New_Line;
         return (null record);
      end Visit_BVH_Leaf;

      function Visit_BVH_Node
        (BB    : Bounding_Box;
         Axis  : Index_3D;
         Left  : BVH;
         Right : BVH) return Empty_Type
      is
         Prefix : String := (1 .. Indent => ' ');
         Dummy  : Empty_Type;
      begin
         Put_Line (Prefix & "Node");
         Put_Line (Prefix & "  BB : " & Image (BB));
         Indent := Indent + 2;
         Dummy := Void_Visitors.Visit
           (Left,
            Visit_BVH_Leaf'Unrestricted_Access,
            Visit_BVH_Node'Unrestricted_Access);
         Dummy := Void_Visitors.Visit
           (Right,
            Visit_BVH_Leaf'Unrestricted_Access,
            Visit_BVH_Node'Unrestricted_Access);
         Indent := Indent - 2;
         return (null record);
      end Visit_BVH_Node;

      Dummy : Empty_Type := Void_Visitors.Visit
        (Root,
         Visit_BVH_Leaf'Unrestricted_Access,
         Visit_BVH_Node'Unrestricted_Access);
   begin
      null;
   end Dump;
end Bounding_Volume_Hierarchies;

