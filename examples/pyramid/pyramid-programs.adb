------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                            Examples Component                            --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2018, Vadim Godunko <vgodunko@gmail.com>                     --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Vadim Godunko, IE nor the names of its        --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------
with League.Characters.Latin;
with League.Strings;

package body Pyramid.Programs is

   use type League.Strings.Universal_String;

   Text_V : League.Strings.Universal_String :=
     League.Strings.To_Universal_String
       ("#version 130") & League.Characters.Latin.Line_Feed &
     "in vec3 vp;" &
     "in vec2 tc;" &
     "out vec2 TexCoord0;" &
     "void main() {" &
     "  gl_Position = vec4(vp, 1.0);" &
     "  TexCoord0 = tc;" &
     "}";
   Text_F : League.Strings.Universal_String :=
     League.Strings.To_Universal_String
       ("#version 130") & League.Characters.Latin.Line_Feed &
     "in vec2 TexCoord0;" &
     "out vec4 frag_colour;" &
     "uniform sampler2D gSampler;" &
     "void main() {" &
     "  frag_colour = texture2D(gSampler, TexCoord0.xy) +vec4(0.0, 0.0, 0.2, 0.2);" &
     "}";

   GS : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("gSampler");
   VP : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("vp");
   TC : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("tc");

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Pyramid_Program'Class) is
   begin
      Self.Add_Shader_From_Source_Code (OpenGL.Vertex, Text_V);
      Self.Add_Shader_From_Source_Code (OpenGL.Fragment, Text_F);
   end Initialize;

   ----------
   -- Link --
   ----------

   overriding function Link (Self : in out Pyramid_Program) return Boolean is
   begin
      if not OpenGL.Programs.OpenGL_Program (Self).Link then
         return False;
      end if;

      Self.GS := Self.Uniform_Location (GS);
      Self.VP := Self.Attribute_Location (VP);
      Self.TC := Self.Attribute_Location (TC);

      return True;
   end Link;

   ----------------------
   -- Set_Texture_Unit --
   ----------------------

   procedure Set_Texture_Unit
    (Self : in out Pyramid_Program'Class;
     Unit : OpenGL.Texture_Unit) is
   begin
      Self.Set_Uniform_Value (Self.GS, Unit);
   end Set_Texture_Unit;

   ----------------------------
   -- Set_Vertex_Data_Buffer --
   ----------------------------

   procedure Set_Vertex_Data_Buffer
    (Self   : in out Pyramid_Program'Class;
     Buffer : Vertex_Data_Buffers.OpenGL_Buffer'Class)
   is
      Dummy : Vertex_Data;

   begin
      Self.Enable_Attribute_Array (Self.VP);
      Self.Enable_Attribute_Array (Self.TC);

      Self.Set_Attribute_Buffer
       (Location   => Self.VP,
        Data_Type  => OpenGL.GL_FLOAT,
        Tuple_Size => Dummy.VP'Length,
        Offset     => Dummy.VP'Position,
        Stride     => Vertex_Data_Buffers.Stride);

      Self.Set_Attribute_Buffer
       (Location   => Self.TC,
        Data_Type  => OpenGL.GL_FLOAT,
        Tuple_Size => Dummy.TC'Length,
        Offset     => Dummy.TC'Position,
        Stride     => Vertex_Data_Buffers.Stride);
   end Set_Vertex_Data_Buffer;

end Pyramid.Programs;
