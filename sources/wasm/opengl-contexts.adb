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

with Web.Strings;

package body OpenGL.Contexts is

   Current : OpenGL_Context_Access;

   ------------
   -- Create --
   ------------

   function Create
    (Self   : in out OpenGL_Context'Class;
     Canvas : Web.HTML.Canvases.HTML_Canvas_Element'Class) return Boolean is
   begin
      Self.Functions.Context :=
        Canvas.Get_Context (Web.Strings.To_Web_String ("webgl"));

      if Self.Functions.Context.Is_Null then
         --  Attempt to create WebGL context with 'old' identifier.

         Self.Functions.Context :=
           Canvas.Get_Context
            (Web.Strings.To_Web_String ("experimental-webgl"));
      end if;

      return not Self.Functions.Context.Is_Null;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
    (Self   : in out OpenGL_Context'Class;
     Canvas : Web.HTML.Canvases.HTML_Canvas_Element'Class) is
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
      if Self.Functions.Context.Is_Null then
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
         null;
         --  XXX Not implemented in Web API
--         Self.Context.Blend_Func
--          (WebAPI.WebGL.GLenum (Source_Factor),
--           WebAPI.WebGL.GLenum (Destination_Factor));
      end Blend_Func;

      -----------
      -- Clear --
      -----------

      overriding procedure Clear
       (Self : WebGL_Functions;
        Mask : OpenGL.Clear_Buffer_Mask)
      is
         use type Web.GL.GLbitfield;

         M : Web.GL.GLbitfield := 0;

      begin
         if OpenGL.Is_Set (Mask, OpenGL.GL_COLOR_BUFFER_BIT) then
            M := M or Web.GL.Rendering_Contexts.COLOR_BUFFER_BIT;
         end if;

         if OpenGL.Is_Set (Mask, OpenGL.GL_DEPTH_BUFFER_BIT) then
            M := M or Web.GL.Rendering_Contexts.DEPTH_BUFFER_BIT;
         end if;

         if OpenGL.Is_Set (Mask, OpenGL.GL_STENCIL_BUFFER_BIT) then
            M := M or Web.GL.Rendering_Contexts.STENCIL_BUFFER_BIT;
         end if;

         declare
            Context_Ref : Web.GL.Rendering_Contexts.WebGL_Rendering_Context
              := Self.Context;

         begin
            Context_Ref.Clear (M);
         end;
      end Clear;

      -----------------
      -- Clear_Color --
      -----------------

      overriding procedure Clear_Color
       (Self  : WebGL_Functions;
        Red   : OpenGL.GLfloat;
        Green : OpenGL.GLfloat;
        Blue  : OpenGL.GLfloat;
        Alpha : OpenGL.GLfloat)
      is
         Context_Ref : Web.GL.Rendering_Contexts.WebGL_Rendering_Context
           := Self.Context;

      begin
         Context_Ref.Clear_Color
          (Web.GL.GLfloat (Red),
           Web.GL.GLfloat (Green),
           Web.GL.GLfloat (Blue),
           Web.GL.GLfloat (Alpha));
      end Clear_Color;

      ----------------
      -- Depth_Func --
      ----------------

      overriding procedure Depth_Func
       (Self : WebGL_Functions;
        Func : OpenGL.GLenum) is
      begin
         null;
         --  XXX Not implemented in Web API

--         Self.Context.Depth_Func (WebAPI.WebGL.GLenum (Func));
      end Depth_Func;

      -------------
      -- Disable --
      -------------

      overriding procedure Disable
       (Self       : WebGL_Functions;
        Capability : OpenGL.GLenum) is
      begin
         null;
         --  XXX Not implemented in Web API

--         Self.Context.Disable (WebAPI.WebGL.GLenum (Capability));
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
         Self.Context.Draw_Arrays
          ((case Mode is
              when GL_POINTS     => Web.GL.Rendering_Contexts.POINTS,
              when GL_LINE_STRIP => Web.GL.Rendering_Contexts.LINE_STRIP,
              when GL_LINE_LOOP  => Web.GL.Rendering_Contexts.LINE_LOOP,
              when GL_LINES      => Web.GL.Rendering_Contexts.LINES,
              when GL_TRIANGLE_STRIP =>
                Web.GL.Rendering_Contexts.TRIANGLE_STRIP,
              when GL_TRIANGLE_FAN =>
                Web.GL.Rendering_Contexts.TRIANGLE_FAN,
              when GL_TRIANGLES  => Web.GL.Rendering_Contexts.TRIANGLES,
              when others        => raise Constraint_Error),
           Web.GL.GLint (First),
           Web.GL.GLsizei (Count));
      end Draw_Arrays;

      -------------------
      -- Draw_Elements --
      -------------------

      overriding procedure Draw_Elements
       (Self      : WebGL_Functions;
        Mode      : OpenGL.GLenum;
        Count     : OpenGL.GLsizei;
        Item_Type : OpenGL.GLenum;
        Offset    : OpenGL.GLintptr) is
      begin
         Self.Context.Draw_Elements
          ((case Mode is
              when GL_POINTS     => Web.GL.Rendering_Contexts.POINTS,
              when GL_LINE_STRIP => Web.GL.Rendering_Contexts.LINE_STRIP,
              when GL_LINE_LOOP  => Web.GL.Rendering_Contexts.LINE_LOOP,
              when GL_LINES      => Web.GL.Rendering_Contexts.LINES,
              when GL_TRIANGLE_STRIP =>
                Web.GL.Rendering_Contexts.TRIANGLE_STRIP,
              when GL_TRIANGLE_FAN =>
                Web.GL.Rendering_Contexts.TRIANGLE_FAN,
              when GL_TRIANGLES  => Web.GL.Rendering_Contexts.TRIANGLES,
              when others        => raise Constraint_Error),
           Count,
           (case Item_Type is
              when GL_UNSIGNED_BYTE => Web.GL.Rendering_Contexts.UNSIGNED_BYTE,
              when GL_UNSIGNED_SHORT =>
                Web.GL.Rendering_Contexts.UNSIGNED_SHORT,
              when others        => raise Constraint_Error),
           Offset);
      end Draw_Elements;

      ------------
      -- Enable --
      ------------

      overriding procedure Enable
       (Self       : WebGL_Functions;
        Capability : OpenGL.GLenum) is
      begin
         null;
         --  XXX Not implemented in Web API

--         Self.Context.Enable (WebAPI.WebGL.GLenum (Capability));
      end Enable;

      ------------
      -- Finish --
      ------------

      overriding procedure Finish (Self : WebGL_Functions) is
         Context_Ref : Web.GL.Rendering_Contexts.WebGL_Rendering_Context
           := Self.Context;

      begin
         Context_Ref.Finish;
      end Finish;

      -----------
      -- Flush --
      -----------

      overriding procedure Flush (Self : WebGL_Functions) is
         Context_Ref : Web.GL.Rendering_Contexts.WebGL_Rendering_Context
           := Self.Context;

      begin
         Context_Ref.Flush;
      end Flush;

      --------------
      -- Viewport --
      --------------

      overriding procedure Viewport
       (Self   : WebGL_Functions;
        X      : OpenGL.GLint;
        Y      : OpenGL.GLint;
        Width  : OpenGL.GLsizei;
        Height : OpenGL.GLsizei)
      is
         Context_Ref : Web.GL.Rendering_Contexts.WebGL_Rendering_Context
           := Self.Context;

      begin
         Context_Ref.Viewport (X, Y, Width, Height);
      end Viewport;

   end WebGL_Functions;

end OpenGL.Contexts;
