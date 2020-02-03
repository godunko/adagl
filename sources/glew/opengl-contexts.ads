------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
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

private with GLFW;
with OpenGL.Functions;

package OpenGL.Contexts is

   pragma Preelaborate;

   type OpenGL_Context is tagged limited private;

   type OpenGL_Context_Access is access all OpenGL_Context'Class;

   function Create
    (Self   : in out OpenGL_Context'Class)
       return Boolean;
   --  Attempts to create the OpenGL context. Returns False on failure.

   procedure Create
    (Self   : in out OpenGL_Context'Class);
   --  Attempts to create the OpenGL context. Raise Program_Error on failure.

   function Functions
    (Self : in out OpenGL_Context'Class)
       return access OpenGL.Functions.OpenGL_Functions'Class;
   --  Returns the OpenGL_Functions implementation for this context.

   procedure Make_Current (Self : in out OpenGL_Context'Class);
   --  Makes the context current.

   function Current_Context return OpenGL_Context_Access;
   --  Returns the last context which called makeCurrent, or null, if no
   --  context is current.

private

   ------------------
   -- My_Functions --
   ------------------

   package My_Functions is

      type My_Functions is
        limited new OpenGL.Functions.OpenGL_Functions with record
         null;
      end record;

      overriding procedure Blend_Func
       (Self               : My_Functions;
        Source_Factor      : OpenGL.GLenum;
        Destination_Factor : OpenGL.GLenum);

      overriding procedure Clear
       (Self : My_Functions;
        Mask : OpenGL.Clear_Buffer_Mask);

      overriding procedure Clear_Color
       (Self  : My_Functions;
        Red   : OpenGL.GLfloat;
        Green : OpenGL.GLfloat;
        Blue  : OpenGL.GLfloat;
        Alpha : OpenGL.GLfloat);

      procedure Clear_Depth
       (Self  : My_Functions;
        Depth : OpenGL.GLfloat);

      overriding procedure Depth_Func
        (Self : My_Functions;
         Func : OpenGL.GLenum);

      overriding procedure Disable
       (Self       : My_Functions;
        Capability : OpenGL.GLenum);

      overriding procedure Draw_Arrays
       (Self  : My_Functions;
        Mode  : OpenGL.GLenum;
        First : OpenGL.GLint;
        Count : OpenGL.GLsizei);

      procedure Draw_Elements
       (Self      : My_Functions;
        Mode      : OpenGL.GLenum;
        Count     : OpenGL.GLsizei;
        Data_Type : OpenGL.GLenum;
        Offset    : OpenGL.GLintptr);

      overriding procedure Enable
       (Self       : My_Functions;
        Capability : OpenGL.GLenum);

      overriding procedure Finish (Self : My_Functions);

      overriding procedure Flush (Self : My_Functions);

      overriding procedure Viewport
       (Self   : My_Functions;
        X      : OpenGL.GLint;
        Y      : OpenGL.GLint;
        Width  : OpenGL.GLsizei;
        Height : OpenGL.GLsizei);

   end My_Functions;

   --------------------
   -- OpenGL_Context --
   --------------------

   type OpenGL_Context is tagged limited record
      Functions : aliased My_Functions.My_Functions;
      Window    : GLFW.GLFWwindow_Access;
   end record;

end OpenGL.Contexts;
