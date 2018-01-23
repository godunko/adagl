------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2016-2018, Vadim Godunko <vgodunko@gmail.com>                --
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
with Ada.Unchecked_Deallocation;

with OpenGL.Contexts.Internals;
with OpenGL.Shaders.Internals;

package body OpenGL.Programs is

   use type WebAPI.WebGL.Programs.WebGL_Program_Access;
   use type WebAPI.WebGL.Rendering_Contexts.WebGL_Rendering_Context_Access;

   procedure Free is
     new Ada.Unchecked_Deallocation
          (OpenGL.Shaders.OpenGL_Shader'Class,
           OpenGL.Shaders.OpenGL_Shader_Access);

   ---------------------------------
   -- Add_Shader_From_Source_Code --
   ---------------------------------

   function Add_Shader_From_Source_Code
    (Self        : in out OpenGL_Program'Class;
     Shader_Type : OpenGL.Shader_Type;
     Source      : League.Strings.Universal_String) return Boolean
   is
      Shader : OpenGL.Shaders.OpenGL_Shader_Access;

   begin
      --  Create WebGL_Program object when necessary.

      if not Self.Create then
         return False;
      end if;

      --  Create shader and compile source code.

      Shader := new OpenGL.Shaders.OpenGL_Shader (Shader_Type);

      if not Shader.Compile_Source_Code (Source) then
         Free (Shader);

         return False;
      end if;

      --  Attach shader to program.

      Self.Context.Attach_Shader
       (Self.Program, OpenGL.Shaders.Internals.Get_WebGL_Shader (Shader));

      return True;
   end Add_Shader_From_Source_Code;

   ---------------------------------
   -- Add_Shader_From_Source_Code --
   ---------------------------------

   procedure Add_Shader_From_Source_Code
    (Self        : in out OpenGL_Program'Class;
     Shader_Type : OpenGL.Shader_Type;
     Source      : League.Strings.Universal_String) is
   begin
      if not Self.Add_Shader_From_Source_Code (Shader_Type, Source) then
         raise Program_Error;
      end if;
   end Add_Shader_From_Source_Code;

   ------------------------
   -- Attribute_Location --
   ------------------------

   function Attribute_Location
    (Self : in out OpenGL_Program'Class;
     Name : League.Strings.Universal_String)
       return OpenGL.Attribute_Location is
   begin
      if Self.Context = null or Self.Program = null then
         return -1;

      else
         return
           OpenGL.Attribute_Location
            (Self.Context.Get_Attrib_Location (Self.Program, Name));
      end if;
   end Attribute_Location;

   ----------
   -- Bind --
   ----------

   function Bind (Self : in out OpenGL_Program'Class) return Boolean is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return False;
      end if;

      if not Self.Is_Linked then
         if not Self.Link then
            --  Unable to link program.

            return False;
         end if;
      end if;

      Self.Context.Use_Program (Self.Program);

      return True;
   end Bind;

   ----------
   -- Bind --
   ----------

   procedure Bind (Self : in out OpenGL_Program'Class) is
   begin
      if not Self.Bind then
         raise Program_Error;
      end if;
   end Bind;

   ------------
   -- Create --
   ------------

   function Create (Self : in out OpenGL_Program'Class) return Boolean is
   begin
      if Self.Context = null then
         Self.Context := OpenGL.Contexts.Internals.Current_WebGL_Context;

         if Self.Context = null then
            return False;
         end if;
      end if;

      if Self.Program = null then
         Self.Program := Self.Context.Create_Program;

         if Self.Program = null then
            return False;
         end if;
      end if;

      return True;
   end Create;

   -----------------------------
   -- Disable_Attribute_Array --
   -----------------------------

   procedure Disable_Attribute_Array
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location) is
   begin
      if Self.Context /= null and Self.Program /= null then
         Self.Context.Disable_Vertex_Attrib_Array
          (WebAPI.WebGL.GLuint (Location));
      end if;
   end Disable_Attribute_Array;

   -----------------------------
   -- Disable_Attribute_Array --
   -----------------------------

   procedure Disable_Attribute_Array
    (Self : in out OpenGL_Program'Class;
     Name : League.Strings.Universal_String)
   is
      Location : constant OpenGL.Attribute_Location
        := Self.Attribute_Location (Name);

   begin
      if Location /= No_Attribute_Location then
         Self.Disable_Attribute_Array (Location);
      end if;
   end Disable_Attribute_Array;

   ----------------------------
   -- Enable_Attribute_Array --
   ----------------------------

   procedure Enable_Attribute_Array
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location) is
   begin
      if Self.Context /= null and Self.Program /= null then
         Self.Context.Enable_Vertex_Attrib_Array
          (WebAPI.WebGL.GLuint (Location));
      end if;
   end Enable_Attribute_Array;

   ----------------------------
   -- Enable_Attribute_Array --
   ----------------------------

   procedure Enable_Attribute_Array
    (Self : in out OpenGL_Program'Class;
     Name : League.Strings.Universal_String)
   is
      Location : constant OpenGL.Attribute_Location
        := Self.Attribute_Location (Name);

   begin
      if Location /= No_Attribute_Location then
         Self.Enable_Attribute_Array (Location);
      end if;
   end Enable_Attribute_Array;

   ---------------
   -- Is_Linked --
   ---------------

   function Is_Linked (Self : OpenGL_Program'Class) return Boolean is
   begin
--      return
--        Self.Context /= null
--          and then Self.Program /= null
--          and then Self.Context.Get_Program_Parameter
--                    (Self.Program,
--                     WebAPI.WebGL.Rendering_Contexts.LINK_STATUS);
      --  XXX A2JS: Short-circuit form must be used here, but not supported.
      return
        Self.Context /= null
          and Self.Program /= null
          and Self.Context.Get_Program_Parameter
               (Self.Program, WebAPI.WebGL.Rendering_Contexts.LINK_STATUS);
   end Is_Linked;

   ----------
   -- Link --
   ----------

   function Link (Self : in out OpenGL_Program) return Boolean is
   begin
      Self.Context.Link_Program (Self.Program);

      if not Self.Context.Get_Program_Parameter
              (Self.Program, WebAPI.WebGL.Rendering_Contexts.LINK_STATUS)
      then
         --  XXX Error handling should be implemented.

         raise Program_Error;
      end if;

      return True;
   end Link;

   -------------
   -- Release --
   -------------

   procedure Release (Self : OpenGL_Program'Class) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      Self.Context.Use_Program (null);
   end Release;

   --------------------------
   -- Set_Attribute_Buffer --
   --------------------------

   procedure Set_Attribute_Buffer
    (Self       : in out OpenGL_Program'Class;
     Location   : OpenGL.Attribute_Location;
     Data_Type  : OpenGL.GLenum;
     Tuple_Size : Positive;
     Offset     : System.Storage_Elements.Storage_Count := 0;
     Stride     : System.Storage_Elements.Storage_Count := 0;
     Normalized : Boolean                               := True) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      Self.Context.Vertex_Attrib_Pointer
       (Index      => WebAPI.WebGL.GLuint (Location),
        Size       => WebAPI.WebGL.GLint (Tuple_Size),
        Data_Type  =>
         (case Data_Type is
            when GL_BYTE           => WebAPI.WebGL.Rendering_Contexts.BYTE,
            when GL_UNSIGNED_BYTE  =>
              WebAPI.WebGL.Rendering_Contexts.UNSIGNED_BYTE,
            when GL_SHORT          => WebAPI.WebGL.Rendering_Contexts.SHORT,
            when GL_UNSIGNED_SHORT =>
               WebAPI.WebGL.Rendering_Contexts.UNSIGNED_SHORT,
            when GL_FLOAT          => WebAPI.WebGL.Rendering_Contexts.FLOAT,
            when others            => WebAPI.WebGL.Rendering_Contexts.BYTE),
--             when others           => raise Constraint_Error),
        Normalized => Normalized,
        Stride     => WebAPI.WebGL.GLsizei (Stride),
        Offset     => WebAPI.WebGL.GLintptr (Offset));
   end Set_Attribute_Buffer;

   --------------------------
   -- Set_Attribute_Buffer --
   --------------------------

   procedure Set_Attribute_Buffer
    (Self       : in out OpenGL_Program'Class;
     Name       : League.Strings.Universal_String;
     Data_Type  : OpenGL.GLenum;
     Tuple_Size : Positive;
     Offset     : System.Storage_Elements.Storage_Count := 0;
     Stride     : System.Storage_Elements.Storage_Count := 0;
     Normalized : Boolean                               := True)
   is
      Location : constant OpenGL.Attribute_Location
        := Self.Attribute_Location (Name);

   begin
      if Location /= No_Attribute_Location then
         Self.Set_Attribute_Buffer
          (Location, Data_Type, Tuple_Size, Offset, Stride, Normalized);
      end if;
   end Set_Attribute_Buffer;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      Self.Context.Vertex_Attrib_1f
       (WebAPI.WebGL.GLuint (Location), WebAPI.WebGL.GLfloat (Value));
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Vector_2) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      Self.Context.Vertex_Attrib_2f
       (WebAPI.WebGL.GLuint (Location),
        WebAPI.WebGL.GLfloat (Value (1)),
        WebAPI.WebGL.GLfloat (Value (2)));
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Vector_3) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      Self.Context.Vertex_Attrib_3f
       (WebAPI.WebGL.GLuint (Location),
        WebAPI.WebGL.GLfloat (Value (1)),
        WebAPI.WebGL.GLfloat (Value (2)),
        WebAPI.WebGL.GLfloat (Value (3)));
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Vector_4) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      Self.Context.Vertex_Attrib_4f
       (WebAPI.WebGL.GLuint (Location),
        WebAPI.WebGL.GLfloat (Value (1)),
        WebAPI.WebGL.GLfloat (Value (2)),
        WebAPI.WebGL.GLfloat (Value (3)),
        WebAPI.WebGL.GLfloat (Value (4)));
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Matrix_2x2) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      Self.Context.Vertex_Attrib_2fv
       (WebAPI.WebGL.GLuint (Location),
        ((WebAPI.WebGL.GLfloat (Value (1, 1)),
          WebAPI.WebGL.GLfloat (Value (1, 2))),
         (WebAPI.WebGL.GLfloat (Value (2, 1)),
          WebAPI.WebGL.GLfloat (Value (2, 2)))));
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Matrix_3x3) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      Self.Context.Vertex_Attrib_2fv
       (WebAPI.WebGL.GLuint (Location),
        ((WebAPI.WebGL.GLfloat (Value (1, 1)),
          WebAPI.WebGL.GLfloat (Value (1, 2)),
          WebAPI.WebGL.GLfloat (Value (1, 3))),
         (WebAPI.WebGL.GLfloat (Value (2, 1)),
          WebAPI.WebGL.GLfloat (Value (2, 2)),
          WebAPI.WebGL.GLfloat (Value (2, 3))),
         (WebAPI.WebGL.GLfloat (Value (3, 1)),
          WebAPI.WebGL.GLfloat (Value (3, 2)),
          WebAPI.WebGL.GLfloat (Value (3, 3)))));
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Matrix_4x4) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      Self.Context.Vertex_Attrib_2fv
       (WebAPI.WebGL.GLuint (Location),
        ((WebAPI.WebGL.GLfloat (Value (1) (1)),
          WebAPI.WebGL.GLfloat (Value (1) (2)),
          WebAPI.WebGL.GLfloat (Value (1) (3)),
          WebAPI.WebGL.GLfloat (Value (1) (4))),
         (WebAPI.WebGL.GLfloat (Value (2) (1)),
          WebAPI.WebGL.GLfloat (Value (2) (2)),
          WebAPI.WebGL.GLfloat (Value (2) (3)),
          WebAPI.WebGL.GLfloat (Value (2) (4))),
         (WebAPI.WebGL.GLfloat (Value (3) (1)),
          WebAPI.WebGL.GLfloat (Value (3) (2)),
          WebAPI.WebGL.GLfloat (Value (3) (3)),
          WebAPI.WebGL.GLfloat (Value (3) (4))),
         (WebAPI.WebGL.GLfloat (Value (4) (1)),
          WebAPI.WebGL.GLfloat (Value (4) (2)),
          WebAPI.WebGL.GLfloat (Value (4) (3)),
          WebAPI.WebGL.GLfloat (Value (4) (4)))));
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat) is
   begin
      Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Vector_2) is
   begin
      Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Vector_3) is
   begin
      Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Vector_4) is
   begin
      Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Matrix_2x2) is
   begin
      Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Matrix_3x3) is
   begin
      Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Matrix_4x4) is
   begin
      Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   end Set_Attribute_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.GLint) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      Self.Context.Uniform_1i
       (WebAPI.WebGL.Uniform_Locations.WebGL_Uniform_Location_Access
         (Location),
        WebAPI.WebGL.GLint (Value));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      Self.Context.Uniform_1f
       (WebAPI.WebGL.Uniform_Locations.WebGL_Uniform_Location_Access
         (Location),
        WebAPI.WebGL.GLfloat (Value));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Vector_2) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

--      Self.Context.Uniform_2fv
--       (WebAPI.WebGL.Uniform_Locations.WebGL_Uniform_Location_Access
--         (Location),
--        (WebAPI.WebGL.GLfloat (Value (1)),
--         WebAPI.WebGL.GLfloat (Value (2))));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Vector_3) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

--      Self.Context.Uniform_3fv
--       (WebAPI.WebGL.Uniform_Locations.WebGL_Uniform_Location_Access
--         (Location),
--        (WebAPI.WebGL.GLfloat (Value (1)),
--         WebAPI.WebGL.GLfloat (Value (2)),
--         WebAPI.WebGL.GLfloat (Value (3))));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Vector_4) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

--      Self.Context.Uniform_4fv
--       (WebAPI.WebGL.Uniform_Locations.WebGL_Uniform_Location_Access
--         (Location),
--        (WebAPI.WebGL.GLfloat (Value (1)),
--         WebAPI.WebGL.GLfloat (Value (2)),
--         WebAPI.WebGL.GLfloat (Value (3)),
--         WebAPI.WebGL.GLfloat (Value (4))));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Matrix_2x2) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      Self.Context.Uniform_Matrix_2fv
       (WebAPI.WebGL.Uniform_Locations.WebGL_Uniform_Location_Access
         (Location),
        False,
        ((WebAPI.WebGL.GLfloat (Value (1, 1)),
          WebAPI.WebGL.GLfloat (Value (1, 2))),
         (WebAPI.WebGL.GLfloat (Value (2, 1)),
          WebAPI.WebGL.GLfloat (Value (2, 2)))));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Matrix_3x3) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      Self.Context.Uniform_Matrix_3fv
       (WebAPI.WebGL.Uniform_Locations.WebGL_Uniform_Location_Access
         (Location),
        False,
        ((WebAPI.WebGL.GLfloat (Value (1, 1)),
          WebAPI.WebGL.GLfloat (Value (1, 2)),
          WebAPI.WebGL.GLfloat (Value (1, 3))),
         (WebAPI.WebGL.GLfloat (Value (2, 1)),
          WebAPI.WebGL.GLfloat (Value (2, 2)),
          WebAPI.WebGL.GLfloat (Value (2, 3))),
         (WebAPI.WebGL.GLfloat (Value (3, 1)),
          WebAPI.WebGL.GLfloat (Value (3, 2)),
          WebAPI.WebGL.GLfloat (Value (3, 3)))));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Matrix_4x4) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      Self.Context.Uniform_Matrix_4fv
       (WebAPI.WebGL.Uniform_Locations.WebGL_Uniform_Location_Access
         (Location),
        False,
        ((WebAPI.WebGL.GLfloat (Value (1) (1)),
          WebAPI.WebGL.GLfloat (Value (1) (2)),
          WebAPI.WebGL.GLfloat (Value (1) (3)),
          WebAPI.WebGL.GLfloat (Value (1) (4))),
         (WebAPI.WebGL.GLfloat (Value (2) (1)),
          WebAPI.WebGL.GLfloat (Value (2) (2)),
          WebAPI.WebGL.GLfloat (Value (2) (3)),
          WebAPI.WebGL.GLfloat (Value (2) (4))),
         (WebAPI.WebGL.GLfloat (Value (3) (1)),
          WebAPI.WebGL.GLfloat (Value (3) (2)),
          WebAPI.WebGL.GLfloat (Value (3) (3)),
          WebAPI.WebGL.GLfloat (Value (3) (4))),
         (WebAPI.WebGL.GLfloat (Value (4) (1)),
          WebAPI.WebGL.GLfloat (Value (4) (2)),
          WebAPI.WebGL.GLfloat (Value (4) (3)),
          WebAPI.WebGL.GLfloat (Value (4) (4)))));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLint) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Vector_2) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Vector_3) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Vector_4) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Matrix_2x2) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Matrix_3x3) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Matrix_4x4) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   ----------------------
   -- Uniform_Location --
   ----------------------

   function Uniform_Location
    (Self : in out OpenGL_Program'Class;
     Name : League.Strings.Universal_String) return OpenGL.Uniform_Location is
   begin
      if Self.Context = null or Self.Program = null then
         return No_Uniform_Location;

      else
         return
           OpenGL.Uniform_Location
            (Self.Context.Get_Uniform_Location (Self.Program, Name));
      end if;
   end Uniform_Location;

end OpenGL.Programs;
