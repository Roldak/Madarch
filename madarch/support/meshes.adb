package body Meshes is
   use GL;

   function Compute_Bounding_Box (M : Mesh) return Bounding_Box is
      use type Ada.Containers.Count_Type;

      From : Singles.Vector3;
      To   : Singles.Vector3;
   begin
      if M.Vertices.Length = 0 then
         return ((others => 0.0), (others => 0.0));
      end if;

      From := M.Vertices (1);
      To   := From;

      for V of M.Vertices loop
         for C in X .. Z loop
            if V (C) < From (C) then
               From (C) := V (C);
            end if;
            if V (C) > To (C) then
               To (C) := V (C);
            end if;
         end loop;
      end loop;

      return (From, To);
   end Compute_Bounding_Box;
end Meshes;
