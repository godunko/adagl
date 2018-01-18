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

with Ada.Unchecked_Deallocation;
with System.Storage_Elements;

with Interfaces.C;

with OpenGL.Contexts.Internals;
with OpenGL.Shaders.Internals;

package body OpenGL.Programs is

   use type GLEW.GLuint;
   use type GLFW.GLFWwindow_Access;

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

      GLEW.glAttachShader
       (Self.Program, OpenGL.Shaders.Internals.Get_GLEW_Shader (Shader));

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
      if Self.Context = null or Self.Program = 0 then
         return -1;

      else
         declare
            Text : Interfaces.C.char_array :=
              Interfaces.C.To_C (Name.To_UTF_8_String);
         begin
            return
              OpenGL.Attribute_Location
                (GLEW.glGetAttribLocation (Self.Program, Text));
         end;
      end if;
   end Attribute_Location;

   ----------
   -- Bind --
   ----------

   function Bind (Self : in out OpenGL_Program'Class) return Boolean is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
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

      GLEW.glUseProgram (Self.Program);

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
         Self.Context := OpenGL.Contexts.Internals.Current_GLFW_Context;

         if Self.Context = null then
            return False;
         end if;
      end if;

      if Self.Program = 0 then
         Self.Program := GLEW.glCreateProgram.all;

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
         GLEW.glDisableVertexAttribArray (GLEW.GLuint (Location));
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
      if Self.Context /= null and Self.Program /= 0 then
         GLEW.glEnableVertexAttribArray (GLEW.GLuint (Location));
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
      Status : aliased OpenGL.GLint;
   begin
      if Self.Context = null or Self.Program = 0 then
         return False;
      else
         GLEW.glGetProgramiv (Self.Program, GLEW.LINK_STATUS, Status'Access);
         return Status /= 0;
      end if;
   end Is_Linked;

   ----------
   -- Link --
   ----------

   function Link (Self : in out OpenGL_Program) return Boolean is
      Status : aliased OpenGL.GLint;
   begin
      GLEW.glLinkProgram (Self.Program);
      GLEW.glGetProgramiv (Self.Program, GLEW.LINK_STATUS, Status'Access);

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
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      GLEW.glUseProgram (0);
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      GLEW.glVertexAttribPointer
       (GLEW.GLuint (Location),
        OpenGL.GLsizei (Tuple_Size),
        Data_Type,
        Boolean'Pos (Normalized),
        OpenGL.GLsizei (Stride),
        System.Storage_Elements.To_Address
          (System.Storage_Elements.Integer_Address (Offset)));
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      GLEW.glVertexAttrib1f (GLEW.GLuint (Location), Value);
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      GLEW.glVertexAttrib2f
       (GLEW.GLuint (Location), Value (1), Value (2));
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      GLEW.glVertexAttrib3f
       (GLEW.GLuint (Location), Value (1), Value (2), Value (3));
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      GLEW.glVertexAttrib4f
       (GLEW.GLuint (Location), Value (1), Value (2), Value (3), Value (4));
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      GLEW.glVertexAttrib2fv (GLEW.GLuint (Location), Value);
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      GLEW.glVertexAttrib3fv (GLEW.GLuint (Location), Value);
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Attribute_Location
      then
         --  Program object is not initialized properly or belongs to another
         --  context.

         return;
      end if;

      GLEW.glVertexAttrib4fv (GLEW.GLuint (Location), Value);
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      GLEW.glUniform1i (OpenGL.GLint (Location), Value);
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      GLEW.glUniform1f (OpenGL.GLint (Location), Value);
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      GLEW.glUniform2f (OpenGL.GLint (Location), Value (1), Value (2));
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      GLEW.glUniform3f
        (OpenGL.GLint (Location), Value (1), Value (2), Value (3));
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      GLEW.glUniform4f
        (OpenGL.GLint (Location), Value (1), Value (2), Value (3), Value (4));
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      GLEW.glUniformMatrix2fv (OpenGL.GLint (Location), 4, 0, Value);
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      GLEW.glUniformMatrix3fv (OpenGL.GLint (Location), 9, 0, Value);
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Location = No_Uniform_Location
      then
         --  Program object is not initialized properly, belongs to another
         --  context, or location is undefined.

         return;
      end if;

      GLEW.glUniformMatrix4fv (OpenGL.GLint (Location), 16, 0, Value);
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
      if Self.Context = null or Self.Program = 0 then
         return No_Uniform_Location;

      else
         declare
            Text : Interfaces.C.char_array :=
              Interfaces.C.To_C (Name.To_UTF_8_String);
         begin
            return
              OpenGL.Uniform_Location
               (GLEW.glGetUniformLocation (Self.Program, Text));
         end;
      end if;
   end Uniform_Location;

end OpenGL.Programs;
