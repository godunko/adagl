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
with OpenGL.Contexts.Internals;
with OpenGL.Textures;

with GLFW;

with Pyramid.Programs;

procedure Pyramid.Driver is

   use type OpenGL.GLfloat;

   Points : constant Pyramid.Programs.Vertex_Data_Array
     := (((0.0, 0.5, 0.0),   (0.5, 1.0)),
         ((0.5, -0.5, 0.0),  (1.0, 0.0)),
         ((-0.5, -0.5, 0.0), (0.0, 0.0)));
   Img    : constant array (1 .. 9, 1 .. 3) of OpenGL.GLubyte :=
     ((255, 0, 0),   (127, 0, 0), (0, 0, 127),
      (127, 255, 0), (0, 127, 0), (0, 0, 127),
      (32, 0, 255),  (0, 0, 127), (0,0, 127));

   Context : OpenGL.Contexts.OpenGL_Context;
   Buffer  :
     Pyramid.Programs.Vertex_Data_Buffers.OpenGL_Buffer (OpenGL.Vertex);
   Program : Pyramid.Programs.Pyramid_Program;
   Texture : OpenGL.Textures.OpenGL_Texture (OpenGL.Texture_2D);

begin
   Context.Create;

   Context.Functions.Enable (OpenGL.GL_DEPTH_TEST);

   Buffer.Create;
   Buffer.Bind;
   Buffer.Allocate (Points);

   Program.Initialize;
   Program.Bind;
   Program.Set_Vertex_Data_Buffer (Buffer);

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

      Window : GLFW.GLFWwindow_Access :=
        OpenGL.Contexts.Internals.Current_GLFW_Context;

   begin
      while GLFW.glfwWindowShouldClose (Window) in 0 loop
         Context.Functions.Clear (Clear_Flag);

         Context.Functions.Draw_Arrays (OpenGL.GL_TRIANGLES, 0, Points'Length);

         GLFW.glfwPollEvents;
         GLFW.glfwSwapBuffers (Window);
      end loop;
   end;

end Pyramid.Driver;
