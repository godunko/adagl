------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2018-2020, Vadim Godunko <vgodunko@gmail.com>                --
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

with Interfaces.C.Strings;
with System.Storage_Elements;
with GLEW;

package body OpenGL.Contexts is

   Current : OpenGL_Context_Access;

   ------------
   -- Create --
   ------------

   function Create
     (Self   : in out OpenGL_Context'Class)
      return Boolean
   is
      use GLFW;

      function glewInit return Interfaces.C.unsigned
        with Import, Convention => C, External_Name => "glewInit";

      name : Interfaces.C.char_array := Interfaces.C.To_C ("Aaa");
      ko   : Interfaces.C.int;
      ok   : Interfaces.C.unsigned;
   begin
      ko := glfwInit;
      pragma Assert (ko in 1);
      Self.Window := glfwCreateWindow (640, 480, name, null, null);
      Self.Make_Current;
      ok := glewInit;
      pragma Assert (ok in 0);

      return True;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Self   : in out OpenGL_Context'Class)
   is
   begin
      if not Self.Create then
         raise Program_Error with "Create fails";
      end if;
   end Create;

   ---------------
   -- Functions --
   ---------------

   function Functions
     (Self : in out OpenGL_Context'Class)
      return access OpenGL.Functions.OpenGL_Functions'Class
   is
   begin
      return Self.Functions'Unchecked_Access;
   end Functions;

   ------------------
   -- Make_Current --
   ------------------

   procedure Make_Current (Self : in out OpenGL_Context'Class) is
   begin
      Current := Self'Unchecked_Access;
      GLFW.glfwMakeContextCurrent (Self.Window);
   end Make_Current;

   ---------------------
   -- Current_Context --
   ---------------------

   function Current_Context return OpenGL_Context_Access is
   begin
      return Current;
   end Current_Context;

   ------------------
   -- My_Functions --
   ------------------

   package body My_Functions is

      ----------------
      -- Blend_Func --
      ----------------

      overriding procedure Blend_Func
        (Self               : My_Functions;
         Source_Factor      : OpenGL.GLenum;
         Destination_Factor : OpenGL.GLenum)
      is
      begin
         GLEW.glBlendFunc (Source_Factor, Destination_Factor);
      end Blend_Func;

      -----------
      -- Clear --
      -----------

      overriding procedure Clear
        (Self : My_Functions;
         Mask : OpenGL.Clear_Buffer_Mask) is
      begin
         GLEW.glClear (Mask);
      end Clear;

      -----------------
      -- Clear_Color --
      -----------------

      overriding procedure Clear_Color
        (Self  : My_Functions;
         Red   : OpenGL.GLfloat;
         Green : OpenGL.GLfloat;
         Blue  : OpenGL.GLfloat;
         Alpha : OpenGL.GLfloat) is
      begin
         GLEW.glClearColor (Red, Green, Blue, Alpha);
      end Clear_Color;

      -----------------
      -- Clear_Depth --
      -----------------

      procedure Clear_Depth
       (Self  : My_Functions;
        Depth : OpenGL.GLfloat) is
      begin
         GLEW.glClearDepth (OpenGL.GLdouble (Depth));
      end Clear_Depth;

      ----------------
      -- Depth_Func --
      ----------------

      overriding procedure Depth_Func
        (Self : My_Functions;
         Func : OpenGL.GLenum) is
      begin
         GLEW.glDepthFunc (Func);
      end Depth_Func;

      -------------
      -- Disable --
      -------------

      overriding procedure Disable
        (Self       : My_Functions;
         Capability : OpenGL.GLenum) is
      begin
         GLEW.glDisable (Capability);
      end Disable;

      -----------------
      -- Draw_Arrays --
      -----------------

      overriding procedure Draw_Arrays
        (Self  : My_Functions;
         Mode  : OpenGL.GLenum;
         First : OpenGL.GLint;
         Count : OpenGL.GLsizei) is
      begin
         GLEW.glDrawArrays (Mode, First, Count);
      end Draw_Arrays;

      -------------------
      -- Draw_Elements --
      -------------------

      procedure Draw_Elements
       (Self      : My_Functions;
        Mode      : OpenGL.GLenum;
        Count     : OpenGL.GLsizei;
        Data_Type : OpenGL.GLenum;
        Offset    : OpenGL.GLintptr) is
      begin
         GLEW.glDrawElements
           (Mode,
            Count,
            Data_Type,
            System.Storage_Elements.To_Address
              (System.Storage_Elements.Integer_Address (Offset)));
      end Draw_Elements;

      ------------
      -- Enable --
      ------------

      overriding procedure Enable
        (Self       : My_Functions;
         Capability : OpenGL.GLenum) is
      begin
         GLEW.glEnable (Capability);
      end Enable;

      ------------
      -- Finish --
      ------------

      overriding procedure Finish (Self : My_Functions) is
      begin
         GLEW.glFinish;
      end Finish;

      -----------
      -- Flush --
      -----------

      overriding procedure Flush (Self : My_Functions) is
      begin
         GLEW.glFlush;
      end Flush;

      --------------
      -- Viewport --
      --------------

      overriding procedure Viewport
        (Self   : My_Functions;
         X      : OpenGL.GLint;
         Y      : OpenGL.GLint;
         Width  : OpenGL.GLsizei;
         Height : OpenGL.GLsizei)
      is
      begin
         GLEW.glViewport (X, Y, Width, Height);
      end Viewport;

   end My_Functions;

end OpenGL.Contexts;
