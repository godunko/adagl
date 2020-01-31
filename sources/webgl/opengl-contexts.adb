------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2016-2020, Vadim Godunko <vgodunko@gmail.com>                --
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

package body OpenGL.Contexts is

   use type WebAPI.WebGL.Rendering_Contexts.WebGL_Rendering_Context_Access;

   Current : OpenGL_Context_Access;

   ------------
   -- Create --
   ------------

   function Create
    (Self   : in out OpenGL_Context'Class;
     Canvas : not null WebAPI.HTML.Canvas_Elements.HTML_Canvas_Element_Access)
       return Boolean is
   begin
      Self.Functions.Context :=
        WebAPI.WebGL.Rendering_Contexts.WebGL_Rendering_Context_Access
         (Canvas.Get_Context (League.Strings.To_Universal_String ("webgl")));

      if Self.Functions.Context = null then
         --  Attempt to create WebGL context with 'old' identifier.

         Self.Functions.Context :=
           WebAPI.WebGL.Rendering_Contexts.WebGL_Rendering_Context_Access
            (Canvas.Get_Context
              (League.Strings.To_Universal_String ("experimental-webgl")));
      end if;

      return Self.Functions.Context /= null;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
    (Self   : in out OpenGL_Context'Class;
     Canvas :
       not null WebAPI.HTML.Canvas_Elements.HTML_Canvas_Element_Access) is
   begin
      if not Self.Create (Canvas) then
         raise Program_Error;
      end if;
   end Create;

   ---------------------
   -- Current_Context --
   ---------------------

   function Current_Context return OpenGL_Context_Access is
   begin
      return Current;
   end Current_Context;

   ---------------
   -- Functions --
   ---------------

   function Functions
    (Self : in out OpenGL_Context'Class)
       return access OpenGL.Functions.OpenGL_Functions'Class is
   begin
      if Self.Functions.Context = null then
         return null;

      else
         return Self.Functions'Unchecked_Access;
      end if;
   end Functions;

   ------------------
   -- Make_Current --
   ------------------

   procedure Make_Current (Self : in out OpenGL_Context'Class) is
   begin
      Current := Self'Unchecked_Access;
   end Make_Current;

   ---------------------
   -- WebGL_Functions --
   ---------------------

   package body WebGL_Functions is

      ----------------
      -- Blend_Func --
      ----------------

      overriding procedure Blend_Func
       (Self               : WebGL_Functions;
        Source_Factor      : OpenGL.GLenum;
        Destination_Factor : OpenGL.GLenum) is
      begin
         Self.Context.Blend_Func
          (WebAPI.WebGL.GLenum (Source_Factor),
           WebAPI.WebGL.GLenum (Destination_Factor));
      end Blend_Func;

      -----------
      -- Clear --
      -----------

      overriding procedure Clear
       (Self : WebGL_Functions;
        Mask : OpenGL.Clear_Buffer_Mask)
      is
         use type WebAPI.WebGL.GLbitfield;

         M : WebAPI.WebGL.GLbitfield := 0;

      begin
         if OpenGL.Is_Set (Mask, OpenGL.GL_COLOR_BUFFER_BIT) then
            M := M or WebAPI.WebGL.Rendering_Contexts.COLOR_BUFFER_BIT;
         end if;

         if OpenGL.Is_Set (Mask, OpenGL.GL_DEPTH_BUFFER_BIT) then
            M := M or WebAPI.WebGL.Rendering_Contexts.DEPTH_BUFFER_BIT;
         end if;

         if OpenGL.Is_Set (Mask, OpenGL.GL_STENCIL_BUFFER_BIT) then
            M := M or WebAPI.WebGL.Rendering_Contexts.STENCIL_BUFFER_BIT;
         end if;

         Self.Context.Clear (M);
      end Clear;

      -----------------
      -- Clear_Color --
      -----------------

      overriding procedure Clear_Color
       (Self  : WebGL_Functions;
        Red   : OpenGL.GLfloat;
        Green : OpenGL.GLfloat;
        Blue  : OpenGL.GLfloat;
        Alpha : OpenGL.GLfloat) is
      begin
         Self.Context.Clear_Color
          (WebAPI.WebGL.GLfloat (Red),
           WebAPI.WebGL.GLfloat (Green),
           WebAPI.WebGL.GLfloat (Blue),
           WebAPI.WebGL.GLfloat (Alpha));
      end Clear_Color;

      -----------------
      -- Clear_Depth --
      -----------------

      overriding procedure Clear_Depth
       (Self  : WebGL_Functions;
        Depth : OpenGL.GLfloat) is
      begin
         Self.Context.Clear_Depth (WebAPI.WebGL.GLfloat (Depth));
      end Clear_Depth;

      ----------------
      -- Depth_Func --
      ----------------

      overriding procedure Depth_Func
       (Self : WebGL_Functions;
        Func : OpenGL.GLenum) is
      begin
         Self.Context.Depth_Func (WebAPI.WebGL.GLenum (Func));
      end Depth_Func;

      -------------
      -- Disable --
      -------------

      overriding procedure Disable
       (Self       : WebGL_Functions;
        Capability : OpenGL.GLenum) is
      begin
         Self.Context.Disable (WebAPI.WebGL.GLenum (Capability));
      end Disable;

      -----------------
      -- Draw_Arrays --
      -----------------

      overriding procedure Draw_Arrays
       (Self  : WebGL_Functions;
        Mode  : OpenGL.GLenum;
        First : OpenGL.GLint;
        Count : OpenGL.GLsizei) is
      begin
         --  XXX A2JS: case expression can be replaced by Unchecked_Conversion
         Self.Context.Draw_Arrays
          ((case Mode is
              when GL_POINTS     => WebAPI.WebGL.Rendering_Contexts.POINTS,
              when GL_LINE_STRIP => WebAPI.WebGL.Rendering_Contexts.LINE_STRIP,
              when GL_LINE_LOOP  => WebAPI.WebGL.Rendering_Contexts.LINE_LOOP,
              when GL_LINES      => WebAPI.WebGL.Rendering_Contexts.LINES,
              when GL_TRIANGLE_STRIP =>
                WebAPI.WebGL.Rendering_Contexts.TRIANGLE_STRIP,
              when GL_TRIANGLE_FAN =>
                WebAPI.WebGL.Rendering_Contexts.TRIANGLE_FAN,
              when GL_TRIANGLES  => WebAPI.WebGL.Rendering_Contexts.TRIANGLES,
              when others => WebAPI.WebGL.Rendering_Contexts.POINTS),
--              when others => raise Constrint_Error),
           WebAPI.WebGL.GLint (First),
           WebAPI.WebGL.GLsizei (Count));
      end Draw_Arrays;

      -------------------
      -- Draw_Elements --
      -------------------

      overriding procedure Draw_Elements
       (Self      : WebGL_Functions;
        Mode      : OpenGL.GLenum;
        Count     : OpenGL.GLsizei;
        Data_Type : OpenGL.GLenum;
        Offset    : OpenGL.GLintptr) is
      begin
         --  XXX A2JS: case expression can be replaced by Unchecked_Conversion
         Self.Context.Draw_Elements
          ((case Mode is
              when GL_POINTS     => WebAPI.WebGL.Rendering_Contexts.POINTS,
              when GL_LINE_STRIP => WebAPI.WebGL.Rendering_Contexts.LINE_STRIP,
              when GL_LINE_LOOP  => WebAPI.WebGL.Rendering_Contexts.LINE_LOOP,
              when GL_LINES      => WebAPI.WebGL.Rendering_Contexts.LINES,
              when GL_TRIANGLE_STRIP =>
                WebAPI.WebGL.Rendering_Contexts.TRIANGLE_STRIP,
              when GL_TRIANGLE_FAN =>
                WebAPI.WebGL.Rendering_Contexts.TRIANGLE_FAN,
              when GL_TRIANGLES  => WebAPI.WebGL.Rendering_Contexts.TRIANGLES,
              when others => WebAPI.WebGL.Rendering_Contexts.POINTS),
--              when others => raise Constrint_Error),
           WebAPI.WebGL.GLsizei (Count),
           (case Data_Type is
              when GL_UNSIGNED_BYTE =>
                WebAPI.WebGL.Rendering_Contexts.UNSIGNED_BYTE,
              when GL_UNSIGNED_SHORT =>
                WebAPI.WebGL.Rendering_Contexts.UNSIGNED_SHORT,
              when others =>
                WebAPI.WebGL.Rendering_Contexts.UNSIGNED_BYTE),
--              when others => raise Constrint_Error),
           WebAPI.WebGL.GLintptr (Offset));
      end Draw_Elements;

      ------------
      -- Enable --
      ------------

      overriding procedure Enable
       (Self       : WebGL_Functions;
        Capability : OpenGL.GLenum) is
      begin
         Self.Context.Enable (WebAPI.WebGL.GLenum (Capability));
      end Enable;

      ------------
      -- Finish --
      ------------

      overriding procedure Finish (Self : WebGL_Functions) is
      begin
         Self.Context.Finish;
      end Finish;

      -----------
      -- Flush --
      -----------

      overriding procedure Flush (Self : WebGL_Functions) is
      begin
         Self.Context.Flush;
      end Flush;

      --------------
      -- Viewport --
      --------------

      overriding procedure Viewport
       (Self   : WebGL_Functions;
        X      : OpenGL.GLint;
        Y      : OpenGL.GLint;
        Width  : OpenGL.GLsizei;
        Height : OpenGL.GLsizei) is
      begin
         Self.Context.Viewport
          (WebAPI.WebGL.GLint (X),
           WebAPI.WebGL.GLint (Y),
           WebAPI.WebGL.GLsizei (Width),
           WebAPI.WebGL.GLsizei (Height));
      end Viewport;

   end WebGL_Functions;

end OpenGL.Contexts;
