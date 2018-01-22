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

with League.Strings;
with League.Characters.Latin;

with GLFW; use GLFW;

with OpenGL.Contexts.Internals;
with OpenGL.Contexts;
with OpenGL.Generic_Buffers;
with OpenGL.Programs;
with OpenGL.Shaders;
with OpenGL.Textures;

procedure Pyramid.Driver is
   use type OpenGL.GLfloat;
   use type League.Strings.Universal_String;

   type Float_Array is array (Positive range <>) of OpenGL.GLfloat;

   package Float_Buffers is new OpenGL.Generic_Buffers
     (OpenGL.GLfloat, Positive, Float_Array);

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
   VP : League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("vp");
   TC : League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("tc");
   Context : OpenGL.Contexts.OpenGL_Context;
   Points  : Float_Array (1 .. 15) :=
     (0.0, 0.5, 0.0,    0.5, 1.0,
      0.5, -0.5, 0.0,   1.0, 0.0,
      -0.5, -0.5, 0.0,  0.0, 0.0);
   Buffer   : Float_Buffers.OpenGL_Buffer (OpenGL.Vertex);
   Program  : OpenGL.Programs.OpenGL_Program;
   Texture  : OpenGL.Textures.OpenGL_Texture (OpenGL.Texture_2D);
   Img      : array (1 .. 9, 1 .. 3) of OpenGL.GLubyte :=
     ((255, 0, 0),   (127, 0, 0), (0, 0, 127),
      (127, 255, 0), (0, 127, 0), (0, 0, 127),
      (32, 0, 255),  (0, 0, 127), (0,0, 127));
begin
   Context.Create;
   Context.Functions.Enable (OpenGL.GL_DEPTH_TEST);
   Buffer.Create;
   Buffer.Bind;
   Buffer.Allocate (Points);

   Program.Add_Shader_From_Source_Code (OpenGL.Vertex, Text_V);
   Program.Add_Shader_From_Source_Code (OpenGL.Fragment, Text_F);

   Program.Bind;

   Program.Set_Uniform_Value
     (Program.Uniform_Location
        (League.Strings.To_Universal_String ("gSampler")), 0);

   Texture.Create;
   Texture.Bind;
   Texture.Set_Image_2D
     (0, OpenGL.GL_RGB, 3, 3, OpenGL.GL_UNSIGNED_BYTE, Img'Address);
   Texture.Set_Parameter (OpenGL.GL_TEXTURE_MIN_FILTER, OpenGL.GL_LINEAR);
   Texture.Set_Parameter (OpenGL.GL_TEXTURE_MAG_FILTER, OpenGL.GL_LINEAR);

   declare
      use type OpenGL.GLbitfield;

      Clear_Flag : OpenGL.Clear_Buffer_Mask :=
        OpenGL.GL_DEPTH_BUFFER_BIT + OpenGL.GL_COLOR_BUFFER_BIT;

      Window : GLFWwindow_Access :=
        OpenGL.Contexts.Internals.Current_GLFW_Context;
   begin
      while glfwWindowShouldClose (Window) in 0 loop
         Context.Functions.Clear (Clear_Flag);
         Program.Enable_Attribute_Array (Program.Attribute_Location (VP));
         Program.Enable_Attribute_Array (Program.Attribute_Location (TC));
         Program.Set_Attribute_Buffer
           (Program.Attribute_Location (VP),
            OpenGL.GL_FLOAT,
            Tuple_Size => 3, Offset => 0, Stride => 20);

         Program.Set_Attribute_Buffer
           (Program.Attribute_Location (TC),
            OpenGL.GL_FLOAT,
            Tuple_Size => 2, Offset => 12, Stride => 20);

         Texture.Bind;
         Context.Functions.Draw_Arrays (OpenGL.GL_TRIANGLES, 0, 3);

         glfwPollEvents;
         glfwSwapBuffers (Window);
      end loop;
   end;

end Pyramid.Driver;
