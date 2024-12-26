--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Unchecked_Deallocation;
with Interfaces.C;

with VSS.Strings.Conversions;

with epoxy_gl_generated_h;
with OpenGL.Contexts.Internals;
with OpenGL.Shaders.Internals;

package body OpenGL.Programs is

   use type Gdk.GLContext.Gdk_GLContext;
   use type epoxy.GLuint;

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
     Source      : VSS.Strings.Virtual_String) return Boolean
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

      epoxy_gl_generated_h.glAttachShader
        (program => Self.Program,
         shader  => OpenGL.Shaders.Internals.Get_Shader_Id (Shader));

      return True;
   end Add_Shader_From_Source_Code;

   ---------------------------------
   -- Add_Shader_From_Source_Code --
   ---------------------------------

   function Add_Shader_From_Source_Code
    (Self        : in out OpenGL_Program'Class;
     Shader_Type : OpenGL.Shader_Type;
     Source      : VSS.String_Vectors.Virtual_String_Vector) return Boolean is
   begin
      return
        Self.Add_Shader_From_Source_Code
          (Shader_Type, Source.Join_Lines (VSS.Strings.LF));
   end Add_Shader_From_Source_Code;

   ---------------------------------
   -- Add_Shader_From_Source_Code --
   ---------------------------------

   procedure Add_Shader_From_Source_Code
    (Self        : in out OpenGL_Program'Class;
     Shader_Type : OpenGL.Shader_Type;
     Source      : VSS.Strings.Virtual_String) is
   begin
      if not Self.Add_Shader_From_Source_Code (Shader_Type, Source) then
         raise Program_Error;
      end if;
   end Add_Shader_From_Source_Code;

   ---------------------------------
   -- Add_Shader_From_Source_Code --
   ---------------------------------

   procedure Add_Shader_From_Source_Code
    (Self        : in out OpenGL_Program'Class;
     Shader_Type : OpenGL.Shader_Type;
     Source      : VSS.String_Vectors.Virtual_String_Vector) is
   begin
      Self.Add_Shader_From_Source_Code
        (Shader_Type, Source.Join_Lines (VSS.Strings.LF));
   end Add_Shader_From_Source_Code;

   ------------------------
   -- Attribute_Location --
   ------------------------

   function Attribute_Location
     (Self : in out OpenGL_Program'Class;
      Name : VSS.Strings.Virtual_String)
      return OpenGL.Attribute_Location is
   begin
      if Self.Context = null or Self.Program = 0 then
         return -1;

      else
         declare
            Text : Interfaces.C.char_array :=
              Interfaces.C.To_C
                (VSS.Strings.Conversions.To_UTF_8_String (Name));

         begin
            return
              OpenGL.Attribute_Location
                (epoxy_gl_generated_h.glGetAttribLocation
                   (Self.Program, Text));
         end;
      end if;
   end Attribute_Location;

   ----------
   -- Bind --
   ----------

   function Bind (Self : in out OpenGL_Program'Class) return Boolean is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context) then
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

      epoxy_gl_generated_h.glUseProgram (Self.Program);

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
         Self.Context := OpenGL.Contexts.Internals.Current_Gdk_Context;

         if Self.Context = null then
            return False;
         end if;
      end if;

      if Self.Program = 0 then
         Self.Program := epoxy_gl_generated_h.glCreateProgram.all;

         if Self.Program = 0 then
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
      if Self.Context /= null and Self.Program /= 0 then
         epoxy_gl_generated_h.glDisableVertexAttribArray
           (epoxy.GLuint (Location));
      end if;
   end Disable_Attribute_Array;

   -----------------------------
   -- Disable_Attribute_Array --
   -----------------------------

   procedure Disable_Attribute_Array
    (Self : in out OpenGL_Program'Class;
     Name : VSS.Strings.Virtual_String)
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
      if Self.Context /= null and Self.Program /= 0 then
         epoxy_gl_generated_h.glEnableVertexAttribArray
           (epoxy.GLuint (Location));
      end if;
   end Enable_Attribute_Array;

   ----------------------------
   -- Enable_Attribute_Array --
   ----------------------------

   procedure Enable_Attribute_Array
    (Self : in out OpenGL_Program'Class;
     Name : VSS.Strings.Virtual_String)
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
      Status : aliased OpenGL.GLint;

   begin
      if Self.Context = null or Self.Program = 0 then
         return False;

      else
         epoxy_gl_generated_h.glGetProgramiv
           (program => Self.Program,
            pname   => epoxy_gl_generated_h.GL_LINK_STATUS,
            params  => Status);

         return Status /= 0;
      end if;
   end Is_Linked;

   ----------
   -- Link --
   ----------

   function Link (Self : in out OpenGL_Program) return Boolean is
      Status : aliased OpenGL.GLint;

   begin
      epoxy_gl_generated_h.glLinkProgram (Self.Program);
      epoxy_gl_generated_h.glGetProgramiv
        (program => Self.Program,
         pname   => epoxy_gl_generated_h.GL_LINK_STATUS,
         params  => Status);

      if Status = 0 then
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
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context) then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      epoxy_gl_generated_h.glUseProgram (0);
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
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      epoxy_gl_generated_h.glVertexAttribPointer
       (index      => epoxy.GLuint (Location),
        size       => OpenGL.GLsizei (Tuple_Size),
        the_type   => Data_Type,
        normalized => Boolean'Pos (Normalized),
        stride     => OpenGL.GLsizei (Stride),
        pointer    =>
          System.Storage_Elements.To_Address
            (System.Storage_Elements.Integer_Address (Offset)));
   end Set_Attribute_Buffer;

   --------------------------
   -- Set_Attribute_Buffer --
   --------------------------

   procedure Set_Attribute_Buffer
    (Self       : in out OpenGL_Program'Class;
     Name       : VSS.Strings.Virtual_String;
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
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      epoxy_gl_generated_h.glVertexAttrib1f (epoxy.GLuint (Location), Value);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Vector_2) is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      epoxy_gl_generated_h.glVertexAttrib2f
        (epoxy.GLuint (Location), Value (1), Value (2));
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Vector_3) is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      epoxy_gl_generated_h.glVertexAttrib3f
        (epoxy.GLuint (Location), Value (1), Value (2), Value (3));
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Vector_4) is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      epoxy_gl_generated_h.glVertexAttrib4f
        (index => epoxy.GLuint (Location),
         x     => Value (1),
         y     => Value (2),
         z     => Value (3),
         w     => Value (4));
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   --  procedure Set_Attribute_Value
   --   (Self     : in out OpenGL_Program'Class;
   --    Location : OpenGL.Attribute_Location;
   --    Value    : OpenGL.GLfloat_Matrix_2x2) is
   --  begin
   --     if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
   --       or Location = No_Attribute_Location
   --     then
   --        --  Program object is not initialized properly or belongs to another
   --        --  context.
   --
   --        return;
   --     end if;
   --
   --     epoxy_gl_generated_h.glVertexAttrib2fv
   --       (epoxy_gl_generated_h.GLuint (Location), Value);
   --  end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   --  procedure Set_Attribute_Value
   --   (Self     : in out OpenGL_Program'Class;
   --    Location : OpenGL.Attribute_Location;
   --    Value    : OpenGL.GLfloat_Matrix_3x3) is
   --  begin
   --     if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
   --       or Location = No_Attribute_Location
   --     then
   --        --  Program object is not initialized properly or belongs to another
   --        --  context.
   --
   --        return;
   --     end if;
   --
   --     epoxy_gl_generated_h.glVertexAttrib3fv (GLEW.GLuint (Location), Value);
   --  end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   --  procedure Set_Attribute_Value
   --   (Self     : in out OpenGL_Program'Class;
   --    Location : OpenGL.Attribute_Location;
   --    Value    : OpenGL.GLfloat_Matrix_4x4) is
   --  begin
   --     if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
   --       or Location = No_Attribute_Location
   --     then
   --        --  Program object is not initialized properly or belongs to another
   --        --  context.
   --
   --        return;
   --     end if;
   --
   --     epoxy_gl_generated_h.glVertexAttrib4fv (GLEW.GLuint (Location), Value);
   --  end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : VSS.Strings.Virtual_String;
     Value : OpenGL.GLfloat) is
   begin
      Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : VSS.Strings.Virtual_String;
     Value : OpenGL.GLfloat_Vector_2) is
   begin
      Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : VSS.Strings.Virtual_String;
     Value : OpenGL.GLfloat_Vector_3) is
   begin
      Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : VSS.Strings.Virtual_String;
     Value : OpenGL.GLfloat_Vector_4) is
   begin
      Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   --  procedure Set_Attribute_Value
   --   (Self  : in out OpenGL_Program'Class;
   --    Name  : League.Strings.Universal_String;
   --    Value : OpenGL.GLfloat_Matrix_2x2) is
   --  begin
   --     Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   --  end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   --  procedure Set_Attribute_Value
   --   (Self  : in out OpenGL_Program'Class;
   --    Name  : League.Strings.Universal_String;
   --    Value : OpenGL.GLfloat_Matrix_3x3) is
   --  begin
   --     Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   --  end Set_Attribute_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   --  procedure Set_Attribute_Value
   --   (Self  : in out OpenGL_Program'Class;
   --    Name  : League.Strings.Universal_String;
   --    Value : OpenGL.GLfloat_Matrix_4x4) is
   --  begin
   --     Self.Set_Attribute_Value (Self.Attribute_Location (Name), Value);
   --  end Set_Attribute_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.GLint) is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      epoxy_gl_generated_h.glUniform1i
        (location => OpenGL.GLint (Location),
         v0       => Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat) is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      epoxy_gl_generated_h.glUniform1f
        (location => OpenGL.GLint (Location),
         v0       => Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Vector_2) is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      epoxy_gl_generated_h.glUniform2f
        (location => OpenGL.GLint (Location),
         v0       => Value (1),
         v1       => Value (2));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Vector_3) is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      epoxy_gl_generated_h.glUniform3f
        (location => OpenGL.GLint (Location),
         v0       => Value (1),
         v1       => Value (2),
         v2       => Value (3));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Vector_4) is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      epoxy_gl_generated_h.glUniform4f
        (location => OpenGL.GLint (Location),
         v0       => Value (1),
         v1       => Value (2),
         v2       => Value (3),
         v3       => Value (4));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Matrix_2x2) is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      epoxy_gl_generated_h.glUniformMatrix2fv
        (location  => OpenGL.GLint (Location),
         count     => 1,
         transpose => epoxy_gl_generated_h.GL_FALSE,
         value     => Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Matrix_3x3) is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      epoxy_gl_generated_h.glUniformMatrix3fv
        (location  => OpenGL.GLint (Location),
         count     => 1,
         transpose => epoxy_gl_generated_h.GL_FALSE,
         value     => Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Matrix_4x4) is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context)
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      epoxy_gl_generated_h.glUniformMatrix4fv
        (location  => OpenGL.GLint (Location),
         count     => 1,
         transpose => epoxy_gl_generated_h.GL_FALSE,
         value     => Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : VSS.Strings.Virtual_String;
     Value : OpenGL.Glfloat) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : VSS.Strings.Virtual_String;
     Value : OpenGL.Glfloat_Vector_2) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : VSS.Strings.Virtual_String;
     Value : OpenGL.Glfloat_Vector_3) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : VSS.Strings.Virtual_String;
     Value : OpenGL.Glfloat_Vector_4) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : VSS.Strings.Virtual_String;
     Value : OpenGL.Glfloat_Matrix_2x2) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : VSS.Strings.Virtual_String;
     Value : OpenGL.Glfloat_Matrix_3x3) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : VSS.Strings.Virtual_String;
     Value : OpenGL.Glfloat_Matrix_4x4) is
   begin
      Self.Set_Uniform_Value (Self.Uniform_Location (Name), Value);
   end Set_Uniform_Value;

   ----------------------
   -- Uniform_Location --
   ----------------------

   function Uniform_Location
    (Self : in out OpenGL_Program'Class;
     Name : VSS.Strings.Virtual_String) return OpenGL.Uniform_Location is
   begin
      if Self.Context = null or Self.Program = 0 then
         return No_Uniform_Location;

      else
         declare
            Text : Interfaces.C.char_array :=
              Interfaces.C.To_C
                (VSS.Strings.Conversions.To_UTF_8_String (Name));
         begin
            return
              OpenGL.Uniform_Location
               (epoxy_gl_generated_h.glGetUniformLocation (Self.Program, Text));
         end;
      end if;
   end Uniform_Location;

end OpenGL.Programs;
