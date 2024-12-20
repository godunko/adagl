--
--  Copyright (C) 2016-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

package body OpenGL is

   ---------
   -- "+" --
   ---------

   function "+" (Right : Clear_Buffer_Mask_Bits) return Clear_Buffer_Mask is
   begin
      return Clear_Buffer_Mask (Right);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+"
    (Left  : Clear_Buffer_Mask_Bits;
     Right : Clear_Buffer_Mask_Bits) return Clear_Buffer_Mask is
   begin
      return Clear_Buffer_Mask (Left) or Clear_Buffer_Mask (Right);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+"
    (Left  : Clear_Buffer_Mask;
     Right : Clear_Buffer_Mask_Bits) return Clear_Buffer_Mask is
   begin
      return Left or Clear_Buffer_Mask (Right);
   end "+";

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : GLfloat_Matrix_4x4;
      Right : GLfloat_Matrix_4x4) return GLfloat_Matrix_4x4 is
   begin
      return Result : GLfloat_Matrix_4x4 do
         for J in GLfloat_Matrix_4x4'Range (1) loop
            for K in GLfloat_Matrix_4x4'Range (2) loop
               Result (J, K) := 0.0;

               for M in GLfloat_Matrix_4x4'Range (2) loop
                  Result (J, K) := @ + Left (J, M) * Right (M, K);
               end loop;
            end loop;
         end loop;
      end return;
   end "*";

   ------------
   -- Is_Set --
   ------------

   function Is_Set
    (Mask : Clear_Buffer_Mask; Bit : Clear_Buffer_Mask_Bits) return Boolean is
   begin
      return (Mask and Clear_Buffer_Mask (Bit)) /= 0;
   end Is_Set;

end OpenGL;
