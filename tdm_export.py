# ***** BEGIN GPL LICENSE BLOCK *****
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# ***** END GPL LICENCE BLOCK *****

import io
import struct
import bmesh  # type:ignore
import os
import bpy  # type:ignore
import math
from bpy.app.handlers import persistent  # type:ignore
from bpy.props import StringProperty, BoolProperty, FloatProperty, EnumProperty  # type:ignore
from bpy_extras.io_utils import ExportHelper  # type:ignore

bl_info = {
    "name": "Dark Mod export formats",
    "author": "The Dark Mod team (originally A. D'Agostino, G. De Roost, R. Bartlett, MCapagnini)",
    "version": (3, 1, 0),
    "blender": (3, 5, 0),
    "location": "File > Export",
    "description": "Export to LWO and ASE formats",
    "category": "Import-Export"
}

# LWO export helper functions

def generate_nstring(string):
    "Generate a null terminated string with an even number of bytes"
    if len(string) % 2 == 0:  # even
        string += "\0\0"
    else:                   # odd
        string += "\0"
    return string


def write_chunk(dest_file, name, data):
    "Write a named LWO chunk to the given file-like object"
    dest_file.write(bytes(name, 'UTF-8'))
    dest_file.write(struct.pack(">L", len(data)))
    dest_file.write(data)


def write_header(dest_file, chunks):
    "Write an LWO header including the size of contained chunks"
    total_chunk_size = sum([len(chunk) for chunk in chunks])
    form_size = total_chunk_size + len(chunks)*8 + len("FORM")
    dest_file.write(b"FORM")
    dest_file.write(struct.pack(">L", form_size))
    dest_file.write(b"LWO2")


def generate_vx(index):
    """Generate and return an LWO-formatted index

    The index is packed either as 16 bits or 32 bits depending on its numerical
    value."""

    if index < 0xFF00:
        return struct.pack(">H", index)                 # 2-byte index
    else:
        return struct.pack(">L", index | 0xFF000000)    # 4-byte index


def generate_vertex_colors(mesh):
    "Generate and return vertex color block"

    alldata = []

    # For each vertex color layer
    for layer in mesh.vertex_colors:

        # Construct output stream for this layer
        data = io.BytesIO()
        data.write(b"RGBA")                                      # type
        data.write(struct.pack(">H", 4))                         # dimension
        data.write(bytes(generate_nstring(layer.name), 'UTF-8'))  # name

        found = False
        for face_idx, face in enumerate(mesh.polygons):
            for vert_idx, loop in zip(face.vertices, face.loop_indices):
                (r, g, b, a) = layer.data[loop].color
                data.write(generate_vx(vert_idx))
                data.write(generate_vx(face_idx))
                data.write(struct.pack(">ffff", r, g, b, a))
                found = True
        if found:
            alldata.append(data.getvalue())

    return alldata


DEFAULT_NAME = "Blender Default"


def generate_default_surf(self):
    "Generate a default mesh surface block"

    data = io.BytesIO()
    material_name = DEFAULT_NAME
    data.write(bytes(generate_nstring(material_name), 'UTF-8'))

    data.write(b"COLR")
    data.write(struct.pack(">H", 14))
    data.write(struct.pack(">fffH", 0.9, 0.9, 0.9, 0))

    data.write(b"DIFF")
    data.write(struct.pack(">H", 6))
    data.write(struct.pack(">fH", 0.8, 0))

    data.write(b"LUMI")
    data.write(struct.pack(">H", 6))
    data.write(struct.pack(">fH", 0, 0))

    data.write(b"SPEC")
    data.write(struct.pack(">H", 6))
    data.write(struct.pack(">fH", 0.4, 0))

    data.write(b"GLOS")
    data.write(struct.pack(">H", 6))
    gloss = 50 / (255/2.0)
    gloss = round(gloss, 1)
    data.write(struct.pack(">fH", gloss, 0))

    return data.getvalue()


def effective_material_name(mat):
    """Extract the name of a Material, overriding it with the FullName custom
    property if present."""
    return mat.get("FullName") or mat.name

def get_mesh_material_names(mesh):
    """Return list of all material names used in the given mesh."""
    if mesh.materials:
        return [effective_material_name(mat) for mat in mesh.materials if mat]
    else:
        return []

def get_used_material_names(meshes):
    """Return a list of all materials used by all given meshes."""
    names = []
    for mesh in meshes:
        names += get_mesh_material_names(mesh)
    return names

class LWOExport(bpy.types.Operator, ExportHelper):
    """Main class implementing export to LWO."""

    bl_idname = "export_scene.lwo"
    bl_label = "LwoExport"
    bl_description = "Export Lightwave .lwo file"
    bl_options = {"REGISTER"}
    filename_ext = ".lwo"
    filter_glob: StringProperty(default="*.lwo", options={'HIDDEN'})

    filepath: StringProperty(
        name="File Path",
        description="File path used for exporting the .lwo file",
        maxlen=1024,
        default="")

    option_smooth: EnumProperty(
        name="Smooth",
        description="How to smooth exported mesh data",
        items=[
            ('NONE', 'None', 'No smoothing information exported'),
            ('FULL', 'Full', 'Entire object is smoothed'),
            ('FROM_OBJECT', 'Use Autosmooth settings',
             'Use Autosmooth checkbox and angle threshold to determine '
             'smoothing of LWO object')
        ],
        default='FROM_OBJECT')

    option_subd: BoolProperty(
        name="Export as subpatched",
        description="Export mesh data as subpatched",
        default=False)

    option_applymod: BoolProperty(
        name="Apply modifiers",
        description="Applies modifiers before exporting",
        default=True)

    option_triangulate: BoolProperty(
        name="Triangulate",
        description="Triangulates all exportable objects",
        default=True)

    option_normals: BoolProperty(
        name="Recalculate Normals",
        description="Recalculate normals before exporting",
        default=False)

    option_remove_doubles: BoolProperty(
        name="Remove Doubles",
        description="Remove any duplicate vertices before exporting",
        default=False)

    option_apply_scale: BoolProperty(
        name="Scale",
        description="Apply scale transformation",
        default=True)

    option_apply_location: BoolProperty(
        name="Location",
        description="Apply location (set model origin to world origin)",
        default=False)

    option_apply_rotation: BoolProperty(
        name="Rotation",
        description="Apply rotation transformation",
        default=True)

    option_batch: BoolProperty(
        name="Batch Export",
        description="A separate .lwo file for every selected object",
        default=False)

    option_scale: FloatProperty(
        name="Scale",
        description="Object scaling factor (default: 1.0)",
        min=0.01,
        max=1000.0,
        soft_min=0.01,
        soft_max=1000.0,
        default=1.0)

    def draw(self, context):
        layout = self.layout

        box = layout.box()

        box.prop(self, 'option_applymod')
        box.prop(self, 'option_subd')
        box.prop(self, 'option_triangulate')
        box.prop(self, 'option_normals')
        box.prop(self, 'option_remove_doubles')
        box.prop(self, 'option_smooth')

        box.separator()
        box.label(text="Transformations:")
        box.prop(self, 'option_apply_scale')
        box.prop(self, 'option_apply_rotation')
        box.prop(self, 'option_apply_location')

        box.label(text="Advanced:")
        box.prop(self, 'option_scale')
        box.prop(self, 'option_batch')

    @classmethod
    def poll(cls, context):
        obj = context.active_object
        return (obj and obj.type == 'MESH')

    def execute(self, context):
        """Main operator execute function."""
        self.context = context

        self.write(self.filepath)

        return {'FINISHED'}

    def write(self, filename):
        """Write objects to an LWO file with the specified filename."""

        objects = list(self.context.selected_objects)
        actobj = self.context.active_object

        objects.sort(key=lambda a: a.name)

        self.meshes = []
        object_name_lookup_orig = {}
        mesh_object_name_lookup = {}  # for name lookups only
        objdups = []

        # Certain operations only work on OBJECT mode
        bpy.ops.object.mode_set(mode='OBJECT')

        # Create duplicates of objects to export, so we can perform operations
        # like removing doubles without modifying the original objects
        for obj in objects:
            if obj.type != 'MESH':
                continue

            bpy.ops.object.select_all(action='DESELECT')
            bpy.context.view_layer.objects.active = obj
            obj.select_set(state=True)
            bpy.ops.object.duplicate()
            objdup = bpy.context.active_object
            objdups.append(objdup)
            object_name_lookup_orig[objdup] = obj.name

            if self.option_applymod:
                if not(objdup.data.shape_keys):                    
                    for mod in 	objdup.modifiers:                           
                        bpy.ops.object.modifier_apply(modifier=mod.name)

                    objdup.modifiers.clear()

            # Options
            bpy.ops.object.mode_set(mode='EDIT')
            if self.option_remove_doubles:
                bpy.ops.object.mode_set(mode='EDIT')
                bpy.ops.mesh.select_all(action='SELECT')
                bpy.ops.mesh.remove_doubles()
            if self.option_triangulate:
                bpy.ops.mesh.select_all(action='SELECT')
                bpy.ops.mesh.quads_convert_to_tris()
            if self.option_normals:
                bpy.ops.object.mode_set(mode='EDIT')
                bpy.ops.mesh.select_all(action='SELECT')
                bpy.ops.mesh.normals_make_consistent()

            # Transformations
            bpy.ops.object.mode_set(mode='OBJECT')
            bpy.ops.object.transform_apply(location=self.option_apply_location,
                                           rotation=self.option_apply_rotation, scale=self.option_apply_scale)

            mesh = objdup.data
            if mesh:
                mesh_object_name_lookup[mesh] = obj.name
                if not(self.option_batch):
                    self.meshes.append(mesh)

        # Export each duplicated object
        for obj in objdups:
            if (self.option_batch):
                self.meshes = [obj.data]

            self.clips = []
            self.clippaths = []
            self.currclipid = 1

            material_names = get_used_material_names(self.meshes)
            tags = self.generate_tags(material_names)
            surfs = []
            chunks = [tags]

            meshdata = io.BytesIO()

            layer_index = 0

            # Generate LWO chunks one mesh at a time
            for i, mesh in enumerate(self.meshes):
                if not(self.option_batch):
                    mobj = objdups[i]

                # Generate a surface chunk for each material in the mesh
                for mat_name in get_mesh_material_names(mesh):
                    surfs.append(self.generate_surface(mesh, mat_name))

                layr = self.generate_layr(
                    mesh_object_name_lookup[mesh], layer_index)
                pnts = self.generate_pnts(mesh)
                bbox = self.generate_bbox(mesh)
                pols = self.generate_pols(mesh, self.option_subd)
                ptag = self.generate_ptag(mesh, material_names)

                if mesh.uv_layers:
                    vmad_uvs = self.generate_vmad_uv(mesh)  # per face

                write_chunk(meshdata, "LAYR", layr)
                chunks.append(layr)
                write_chunk(meshdata, "PNTS", pnts)
                chunks.append(pnts)
                write_chunk(meshdata, "BBOX", bbox)
                chunks.append(bbox)

                if mesh.vertex_colors:
                    vcs = generate_vertex_colors(mesh)  # per vert
                    for vmad in vcs:
                        write_chunk(meshdata, "VMAD", vmad)
                        chunks.append(vmad)
                write_chunk(meshdata, "POLS", pols)
                chunks.append(pols)
                write_chunk(meshdata, "PTAG", ptag)
                chunks.append(ptag)

                if mesh.uv_layers:
                    for vmad in vmad_uvs:
                        write_chunk(meshdata, "VMAD", vmad)
                        chunks.append(vmad)

                layer_index += 1

            for clip in self.clips:
                chunks.append(clip)
            for surf in surfs:
                chunks.append(surf)

            # Prepare the output file
            if (self.option_batch):
                filename = os.path.dirname(filename)
                filename += (os.sep +
                             object_name_lookup_orig[obj].replace('.', '_'))
            if not filename.lower().endswith('.lwo'):
                filename += '.lwo'

            # Write generated chunk data to the output file
            with open(filename, "wb") as outfile:
                write_header(outfile, chunks)
                write_chunk(outfile, "TAGS", tags)
                outfile.write(meshdata.getvalue())
                meshdata.close()
                for clip in self.clips:
                    write_chunk(outfile, "CLIP", clip)
                for surf in surfs:
                    write_chunk(outfile, "SURF", surf)

            bpy.ops.object.select_all(action='DESELECT')
            bpy.context.view_layer.objects.active = obj
            obj.select_set(state=True)
            bpy.ops.object.delete()

            if not(self.option_batch):
                # if not batch exporting, all meshes of objects are already saved
                break

        for obj in objects:
            obj.select_set(state=True)
        bpy.context.view_layer.objects.active = actobj

    # =========================================
    # === Generate Tag Strings (TAGS Chunk) ===
    # =========================================

    def generate_tags(self, material_names):
        data = io.BytesIO()
        if material_names:
            for mat in material_names:
                data.write(bytes(generate_nstring(mat), 'UTF-8'))
            return data.getvalue()
        else:
            return generate_nstring('')

    def generate_mesh_surface(self, mesh, material_name):
        "Generate and return mesh surface block"

        data = io.BytesIO()
        data.write(bytes(generate_nstring(material_name), 'UTF-8'))

        # Extract basic material information
        try:
            material = bpy.data.materials.get(material_name)
            R, G, B = material.diffuse_color[0], material.diffuse_color[1], material.diffuse_color[2]
            spec = material.specular_intensity
        except Exception as e:
            print("generate_mesh_surface() caught exception: {}".format(e))
            material = None
            spec = 0.0
            R = G = B = 1.0

        # Extract smoothing angle
        if self.option_smooth == 'FULL':
            sman = math.pi
        elif mesh.use_auto_smooth:
            sman = mesh.auto_smooth_angle
        else:
            sman = 0.0

        data.write(b"COLR")
        data.write(struct.pack(">H", 0))

        data.write(b"COLR")
        data.write(struct.pack(">H", 14))
        data.write(struct.pack(">fffH", R, G, B, 0))

        data.write(b"DIFF")
        data.write(struct.pack(">H", 6))
        data.write(struct.pack(">fH", 1.0, 0))

        data.write(b"LUMI")
        data.write(struct.pack(">H", 6))
        data.write(struct.pack(">fH", 0.0, 0))

        data.write(b"SPEC")
        data.write(struct.pack(">H", 6))
        data.write(struct.pack(">fH", spec, 0))

        # Write smoothing information if required
        if self.option_smooth != 'NONE':
            data.write(b"SMAN")
            data.write(struct.pack(">H", 4))
            data.write(struct.pack(">f", sman))

        return data.getvalue()

    # ========================
    # === Generate Surface ===
    # ========================
    def generate_surface(self, mesh, name):
        if name == DEFAULT_NAME:
            return generate_default_surf()
        else:
            return self.generate_mesh_surface(mesh, name)

    # ===================================
    # === Generate Layer (LAYR Chunk) ===
    # ===================================
    def generate_layr(self, name, idx):
        px, py, pz = bpy.data.objects.get(name).location
        data = io.BytesIO()
        data.write(struct.pack(">h", idx))          # layer number
        data.write(struct.pack(">h", 0))            # flags
        data.write(struct.pack(">fff", px, pz, py))  # pivot
        data.write(bytes(generate_nstring(
            name.replace(" ", "_").replace(".", "_")), 'UTF-8'))
        return data.getvalue()

    # ===================================
    # === Generate Verts (PNTS Chunk) ===
    # ===================================
    def generate_pnts(self, mesh):
        data = io.BytesIO()
        for i, v in enumerate(mesh.vertices):
            x, y, z = v.co
            x *= self.option_scale
            y *= self.option_scale
            z *= self.option_scale
            data.write(struct.pack(">fff", x, z, y))
        return data.getvalue()

    # ==========================================
    # === Generate Bounding Box (BBOX Chunk) ===
    # ==========================================
    def generate_bbox(self, mesh):
        data = io.BytesIO()
        # need to transform verts here
        if mesh.vertices:
            nv = [v.co for v in mesh.vertices]
            xx = [co[0] * self.option_scale for co in nv]
            yy = [co[1] * self.option_scale for co in nv]
            zz = [co[2] * self.option_scale for co in nv]
        else:
            xx = yy = zz = [0.0, ]

        data.write(struct.pack(">6f", min(xx), min(zz),
                               min(yy), max(xx), max(zz), max(yy)))
        return data.getvalue()

    # ================================================
    # === Generate Per-Face UV Coords (VMAD Chunk) ===
    # ================================================
    def generate_vmad_uv(self, mesh):
        alldata = []
        layers = mesh.uv_layers
        for l in layers:
            uvname = generate_nstring(l.name)
            data = io.BytesIO()
            data.write(b"TXUV")                                      # type
            data.write(struct.pack(">H", 2))                         # dimension
            data.write(bytes(uvname, 'UTF-8'))  # name

            found = False
            for i, p in enumerate(mesh.polygons):
                for v, loop in zip(p.vertices, p.loop_indices):
                    searchl = list(p.loop_indices)
                    searchl.extend(list(p.loop_indices))
                    pos = searchl.index(loop)
                    prevl = searchl[pos - 1]
                    nextl = searchl[pos + 1]
                    youv = l.data[loop].uv
                    if l.data[prevl].uv == youv == l.data[nextl].uv:
                        continue
                    data.write(generate_vx(v))  # vertex index
                    data.write(generate_vx(i))  # face index
                    data.write(struct.pack(">ff", youv[0], youv[1]))
                    found = True
            if found:
                alldata.append(data.getvalue())

        return alldata

    # ===================================
    # === Generate Faces (POLS Chunk) ===
    # ===================================
    def generate_pols(self, mesh, subd):
        data = io.BytesIO()
        if subd:
            data.write(b"SUBD")  # subpatch polygon type
        else:
            data.write(b"FACE")  # normal polygon type
        for i, p in enumerate(mesh.polygons):
            data.write(struct.pack(">H", len(p.vertices)))  # numfaceverts
            numfaceverts = len(p.vertices)
            p_vi = p.vertices
            for j in range(numfaceverts-1, -1, -1):         # Reverse order
                data.write(generate_vx(p_vi[j]))
        bm = bmesh.new()
        bm.from_mesh(mesh)
        for e in bm.edges:
            if len(e.link_faces) == 0:
                data.write(struct.pack(">H", 2))
                data.write(generate_vx(e.verts[0].index))
                data.write(generate_vx(e.verts[1].index))
        bm.to_mesh(mesh)

        return data.getvalue()

    # =================================================
    # === Generate Polygon Tag Mapping (PTAG Chunk) ===
    # =================================================
    def generate_ptag(self, mesh, material_names):
        data = io.BytesIO()
        data.write(b"SURF")
        for poly in mesh.polygons:
            if mesh.materials:
                matindex = poly.material_index
                matname = effective_material_name(mesh.materials[matindex])
                surfindex = material_names.index(matname)

                data.write(generate_vx(poly.index))
                data.write(struct.pack(">H", surfindex))
            else:
                data.write(generate_vx(poly.index))
                data.write(struct.pack(">H", 0))
        return data.getvalue()

# settings
def aseFloat(x): return '''{0:0.4f}'''.format(x)

optionScale = 16.0
optionSubmaterials = False
optionSmoothingGroups = True
optionAllowMultiMats = True

# ASE components
aseHeader = ''
aseScene = ''
aseMaterials = ''
aseGeometry = ''

# Other
matList = []
numMats = 0
currentMatId = 0

class Error(Exception):
    def __init__(self, message):
        self.message = message
        print('\n\n' + message + '\n\n')

class cHeader:
    def __init__(self):
        self.comment = "Ascii Scene Exporter v2.52"

    def __repr__(self):
        return '''*3DSMAX_ASCIIEXPORT\t200\n*COMMENT "{0}"\n'''.format(self.comment)

class cScene:
    def __init__(self):
        self.filename = bpy.path.basename(bpy.data.filepath)
        self.firstframe = 0
        self.lastframe = 100
        self.framespeed = 30
        self.ticksperframe = 160
        self.backgroundstatic = '\t'.join(
            [aseFloat(x) for x in [0.0, 0.0, 0.0]])
        self.ambientstatic = '\t'.join([aseFloat(x) for x in [0.0, 0.0, 0.0]])

    def __repr__(self):
        return ("*SCENE {{\n\t*SCENE_FILENAME \"{0}\"" +
                "\n\t*SCENE_FIRSTFRAME {1}" +
                "\n\t*SCENE_LASTFRAME {2}" +
                "\n\t*SCENE_FRAMESPEED {3}" +
                "\n\t*SCENE_TICKSPERFRAME {4}" +
                "\n\t*SCENE_BACKGROUND_STATIC {5}" +
                "\n\t*SCENE_AMBIENT_STATIC {6}" +
                "\n}}\n").format(self.filename, self.firstframe, self.lastframe, self.framespeed, self.ticksperframe, self.backgroundstatic, self.ambientstatic)

class ASEMaterials:
    def _write_materials(self, material_list):
        """Generate text representing materials in ASE format."""

        num_mats = len(material_list)
        result = f"*MATERIAL_LIST {{\n\t*MATERIAL_COUNT {num_mats}"
        for index, slot in enumerate(material_list):
            result += '\n' + str(ASEMaterial(index, slot))
        result += '\n}'
        return result

    def __init__(self, objects):
        global optionSubmaterials
        global matList
        global numMats

        self.material_list = []

        # Get all of the materials used by non-collision object meshes
        for object in objects:
            if collisionObject(object) == 2:
                continue
            elif object.type != 'MESH':
                continue
            else:
                print(object.name + ': Constructing Materials')
                for slot in object.material_slots:
                    # if the material is not in the material_list, add it
                    if self.material_list.count(slot.material) == 0:
                        self.material_list.append(slot.material)
                        matList.append(slot.material.name)

        self.material_count = len(self.material_list)
        numMats = self.material_count

        # Raise an error if there are no materials found
        if self.material_count == 0:
            raise Error('Mesh must have at least one applied material')
        else:
            if (optionSubmaterials):
                self.dump = cSubMaterials(self.material_list)
            else:
                self.dump = self._write_materials(self.material_list)

    def __repr__(self):
        return str(self.dump)


class cSubMaterials:
    def __init__(self, material_list):
        slot = material_list[0]
        # Initialize material information
        self.dump = ("*MATERIAL_LIST {" +
                     "\n\t*MATERIAL_COUNT 1" +
                     "\n\t*MATERIAL 0 {")
        self.matDump = ''
        self.name = material_list[0].name
        self.numSubMtls = len(material_list)
        self.diffusemap = ASEDiffuseMap(slot)
        if (self.numSubMtls > 1):
            self.matClass = 'Multi/Sub-Object'
            self.diffuseDump = ''
        else:
            self.matClass = 'Standard'
            self.numSubMtls = 0
            self.diffuseDump = self.diffdump()
        self.ambient = '\t'.join([aseFloat(x) for x in [0.0, 0.0, 0.0]])
        self.diffuse = '\t'.join([aseFloat(x) for x in slot.diffuse_color])
        self.specular = '\t'.join([aseFloat(x) for x in slot.specular_color])
        self.shine = aseFloat(1.0 / slot.roughness)
        self.shinestrength = aseFloat(slot.specular_intensity)
        self.transparency = aseFloat(0.0)
        self.wiresize = aseFloat(1.0)
        self.shading = 'Blinn'
        self.xpfalloff = aseFloat(0.0)
        self.xptype = 'Filter'
        self.falloff = 'In'
        self.soften = False
        self.submtls = []
        self.selfillum = aseFloat(0.0)

        if (len(material_list) > 1):
            # Build SubMaterials
            for index, slot in enumerate(material_list):
                self.matDump += ("\n\t\t*SUBMATERIAL {0} {{" +
                                 "{1}" +
                                 "\n\t\t}}").format(index, ASEMaterial(slot))

        self.dump += ("\n\t\t*MATERIAL_NAME \"{0}\"" +
                      "\n\t\t*MATERIAL_CLASS \"{1}\"" +
                      "\n\t\t*MATERIAL_AMBIENT {2}" +
                      "\n\t\t*MATERIAL_DIFFUSE {3}" +
                      "\n\t\t*MATERIAL_SPECULAR {4}" +
                      "\n\t\t*MATERIAL_SHINE {5}" +
                      "\n\t\t*MATERIAL_SHINESTRENGTH {6}" +
                      "\n\t\t*MATERIAL_TRANSPARENCY {7}" +
                      "\n\t\t*MATERIAL_WIRESIZE {8}" +
                      "\n\t\t*MATERIAL_SHADING {9}" +
                      "\n\t\t*MATERIAL_XP_FALLOFF {10}" +
                      "\n\t\t*MATERIAL_SELFILLUM {11}" +
                      "\n\t\t*MATERIAL_FALLOFF {12}" +
                      "\n\t\t*MATERIAL_XP_TYPE {13}" +
                      "{14}" +
                      "\n\t\t*NUMSUBMTLS {15}" +
                      "{16}").format(self.name, self.matClass, self.ambient, self.diffuse, self.specular, self.shine, self.shinestrength, self.transparency, self.wiresize, self.shading, self.xpfalloff, self.selfillum, self.falloff, self.xptype, self.diffuseDump, self.numSubMtls, self.matDump)

        self.dump += '\n\t}'
        self.dump += '\n}'

    def diffdump(self):
        for x in [self.diffusemap]:
            return x

    def __repr__(self):
        return self.dump


class ASEDecl(object):
    """A named block in an ASE file.

    Each ASE decl consists of a starred uppercase block title followed by an
    open brace, then any number of sub-declarations or key/value lines, then a
    closing brace.

    For example:

    *BLOCK_TITLE {
        *FIRST_KEY 1.0
        *SECOND_KEY 2.5
    }

    Each line within the decl block is indented relative to the block's own
    indent level.
    """

    def __init__(self, name, indent_level=0):
        """Initialise an ASEDecl specifying an indent level if needed."""
        self.__name = name
        self.__indent = indent_level
        self.__header = indent_line("*{0} {{".format(name), self.__indent)
        self.__lines = []
        self.__footer = indent_line("}", self.__indent)

    def _add_line(self, line):
        """Add an indented line to the internal list of lines.

        The line is indented by one level relative to the ASEDecl's own base indent."""

        self.__lines.append(indent_line(line, self.__indent + 1))

    def get_name(self):
        """Return the name of this decl."""
        return self.__name

    def get_lines(self):
        """Return the list of lines in this decl, excluding the header and footer."""
        return self.__lines

    def add_value(self, key, value):
        """Add a line to this block."""

        # If the value is a float, format it with a fixed number of decimal
        # places. Otherwise insert it using its own string representation.
        if isinstance(value, float):
            value_str = aseFloat(value)
        else:
            value_str = str(value)

        # Construct and append the indented line
        self._add_line(f"*{key.upper()} {value_str}")

    def add_decl(self, decl):
        """Add a subordinate ASEDecl to this decl.

        The decl name takes the place of a key, and the contents of the decl
        (enclosed in braces) take the place of a value, e.g.

        *MAIN_DECL {
            *KEY 1.0
            *SUB_DECL {
                *SUB_KEY 2.5
            }
        }"""
        self._add_line(f"*{decl.get_name()} {{")
        for l in decl.get_lines():
            # decl lines are already indented, to which we add our own indent
            self._add_line(l)
        self._add_line("}")

    def __str__(self):
        """Convert the ASEDecl into its final string representation."""
        return '\n'.join([self.__header] + self.__lines + [self.__footer])


class ASEMaterial(ASEDecl):
    """Representation of a single material in ASE format."""

    def __init__(self, material_num, material):
        """Initialise with material index and a bpy.Material object to export."""

        super().__init__(f"MATERIAL {material_num}", 1)

        ambient = '\t'.join([aseFloat(x) for x in [0.0, 0.0, 0.0]])
        diffuse = '\t'.join([aseFloat(x) for x in material.diffuse_color])
        specular = '\t'.join([aseFloat(x) for x in material.specular_color])

        # Material Definition
        self.add_value("MATERIAL_NAME", Quoted(effective_material_name(material)))
        self.add_value("MATERIAL_CLASS", Quoted("Standard"))
        self.add_value("MATERIAL_AMBIENT", ambient)
        self.add_value("MATERIAL_DIFFUSE", diffuse)
        self.add_value("MATERIAL_SPECULAR", specular)
        self.add_value("MATERIAL_SHINE", 1.0 / material.roughness)
        self.add_value("MATERIAL_SHINESTRENGTH", material.specular_intensity)
        self.add_value("MATERIAL_TRANSPARENCY", 0.0)
        self.add_value("MATERIAL_WIRESIZE", 1.0)
        self.add_value("MATERIAL_SHADING", "Blinn")
        self.add_value("MATERIAL_XP_FALLOFF", 0.0)
        self.add_value("MATERIAL_SELFILLUM", 0.0)
        self.add_value("MATERIAL_FALLOFF", "In")
        self.add_value("MATERIAL_XP_TYPE", "Filter")
        self.add_decl(ASEDiffuseMap(material))


def indent_line(text, indent_level=0):
    """Indent a line by the specified number of tabs."""
    return ('\t' * indent_level) + text


class Quoted(object):
    """Wrapper for a string which adds quotes when serialised."""

    def __init__(self, string):
        self.__str = string

    def __str__(self):
        return '"{0}"'.format(self.__str)


class ASEDiffuseMap(ASEDecl):
    "Representation of diffuse map properties in ASE string format"

    def __init__(self, material):
        """Initialise diffusemap data with a material slot."""

        super().__init__("MAP_DIFFUSE")

        if material:
            name = effective_material_name(material)
            bitmap = '//base/' + name
        else:
            name = 'default'
            bitmap = 'None'

        self.add_value("MAP_NAME", Quoted(name))
        self.add_value("MAP_CLASS", Quoted('Bitmap'))
        self.add_value("MAP_SUBNO", 1)
        self.add_value("MAP_AMOUNT", 1.0)
        self.add_value("BITMAP", Quoted(bitmap))
        self.add_value("MAP_TYPE", 'Screen')
        self.add_value("UVW_U_OFFSET", 0.0)
        self.add_value("UVW_V_OFFSET", 0.0)
        self.add_value("UVW_U_TILING", 1.0)
        self.add_value("UVW_V_TILING", 1.0)
        self.add_value("UVW_ANGLE", 0.0)
        self.add_value("UVW_BLUR", 1.0)
        self.add_value("UVW_BLUR_OFFSET", 0.0)
        self.add_value("UVW_NOISE_AMT", 1.0)
        self.add_value("UVW_NOISE_SIZE", 1.0)
        self.add_value("UVW_NOISE_LEVEL", 1)
        self.add_value("UVW_NOISE_PHASE", 0.0)
        self.add_value("BITMAP_FILTER", 'Pyramidal')

class cGeomObject:
    def __init__(self, object):
        print(object.name + ": Constructing Geometry")
        global optionAllowMultiMats
        global currentMatId

        self.name = object.name
        self.prop_motionblur = 0
        self.prop_castshadow = 1
        self.prop_recvshadow = 1

        if optionAllowMultiMats:
            self.material_ref = 0
        else:
            self.material_ref = currentMatId

        self.nodetm = cNodeTM(object)
        self.mesh = cMesh(object)

        self.dump = '''\n*GEOMOBJECT {{\n\t*NODE_NAME "{0}"\n{1}\n{2}\n\t*PROP_MOTIONBLUR {3}\n\t*PROP_CASTSHADOW {4}\n\t*PROP_RECVSHADOW {5}\n\t*MATERIAL_REF {6}\n}}'''.format(
            self.name, self.nodetm, self.mesh, self.prop_motionblur, self.prop_castshadow, self.prop_recvshadow, self.material_ref)

    def __repr__(self):
        return self.dump


class cNodeTM:
    def __init__(self, object):
        self.name = object.name
        self.inherit_pos = '0 0 0'
        self.inherit_rot = '0 0 0'
        self.inherit_scl = '0 0 0'
        self.tm_row0 = '\t'.join([aseFloat(x) for x in [1.0, 0.0, 0.0]])
        self.tm_row1 = '\t'.join([aseFloat(x) for x in [0.0, 1.0, 0.0]])
        self.tm_row2 = '\t'.join([aseFloat(x) for x in [0.0, 0.0, 1.0]])
        self.tm_row3 = '\t'.join([aseFloat(x) for x in [0.0, 0.0, 0.0]])
        self.tm_pos = '\t'.join([aseFloat(x) for x in [0.0, 0.0, 0.0]])
        self.tm_rotaxis = '\t'.join([aseFloat(x) for x in [0.0, 0.0, 0.0]])
        self.tm_rotangle = aseFloat(0.0)
        self.tm_scale = '\t'.join([aseFloat(x) for x in [1.0, 1.0, 1.0]])
        self.tm_scaleaxis = '\t'.join([aseFloat(x) for x in [0.0, 0.0, 0.0]])
        self.tm_scaleaxisang = aseFloat(0.0)

        self.dump = ("\t*NODE_TM {{" +
                     "\n\t\t*NODE_NAME \"{0}\"" +
                     "\n\t\t*INHERIT_POS {1}" +
                     "\n\t\t*INHERIT_ROT {2}" +
                     "\n\t\t*INHERIT_SCL {3}" +
                     "\n\t\t*TM_ROW0 {4}" +
                     "\n\t\t*TM_ROW1 {5}" +
                     "\n\t\t*TM_ROW2 {6}" +
                     "\n\t\t*TM_ROW3 {7}" +
                     "\n\t\t*TM_POS {8}" +
                     "\n\t\t*TM_ROTAXIS {9}" +
                     "\n\t\t*TM_ROTANGLE {10}" +
                     "\n\t\t*TM_SCALE {11}" +
                     "\n\t\t*TM_SCALEAXIS {12}" +
                     "\n\t\t*TM_SCALEAXISANG {13}" +
                     "\n\t}}").format(self.name, self.inherit_pos, self.inherit_rot, self.inherit_scl, self.tm_row0, self.tm_row1, self.tm_row2, self.tm_row3, self.tm_pos, self.tm_rotaxis, self.tm_rotangle, self.tm_scale, self.tm_scaleaxis, self.tm_scaleaxisang)

    def __repr__(self):
        return self.dump


class cMesh:
    def __init__(self, obj):
        bpy.ops.mesh.reveal

        self.uvdata = cUVdata(obj)

        self.timevalue = '0'
        self.numvertex = len(obj.data.vertices)
        self.numfaces = len(obj.data.polygons)
        self.vertlist = cVertlist(obj)
        self.facelist = cFacelist(obj)

        # Vertex Paint
        if len(obj.data.vertex_colors) > 0:
            self.cvertlist = cCVertlist(obj)
            self.numcvertex = self.cvertlist.length
            self.numcvfaces = len(obj.data.vertex_colors.data.polygons)
            self.cfacelist = cCFacelist(self.numcvfaces)
            # change them into strings now
            self.numcvertex = '\n\t\t*MESH_NUMCVERTEX {0}'.format(
                self.numcvertex)
            self.cvertlist = '\n{0}'.format(self.cvertlist)
            self.numcvfaces = '\n\t\t*MESH_NUMCVFACES {0}'.format(
                self.numcvfaces)
            self.cfacelist = '\n{0}'.format(self.cfacelist)
        else:
            self.numcvertex = '\n\t\t*MESH_NUMCVERTEX 0'
            self.cvertlist = ''
            self.numcvfaces = ''
            self.cfacelist = ''

        self.normals = cNormallist(obj)

    def __repr__(self):
        temp = '''\t*MESH {{\n\t\t*TIMEVALUE {0}\n\t\t*MESH_NUMVERTEX {1}\n\t\t*MESH_NUMFACES {2}\n\t\t*MESH_VERTEX_LIST {3}\n\t\t*MESH_FACE_LIST {4}{5}{6}{7}{8}{9}\n{10}\n\t}}'''.format(
            self.timevalue, self.numvertex, self.numfaces, self.vertlist, self.facelist, self.uvdata, self.numcvertex, self.cvertlist, self.numcvfaces, self.cfacelist, self.normals)
        return temp


class cVertlist:
    def __init__(self, object):
        self.vertlist = []
        for data in object.data.vertices:
            temp = cVert(data.index, data.co.to_tuple(4))
            self.vertlist.append(temp)

    def dump(self):
        temp = ''
        for x in self.vertlist:
            temp += str(x)
        return temp

    def __repr__(self):
        return '''{{\n{0}\t\t}}'''.format(self.dump())


class cVert:
    def __init__(self, index, coord):
        global optionScale

        self.index = index
        self.x = aseFloat(coord[0] * optionScale)
        self.y = aseFloat(coord[1] * optionScale)
        self.z = aseFloat(coord[2] * optionScale)

    def __repr__(self):
        return '''\t\t\t*MESH_VERTEX {0:4d}\t{1}\t{2}\t{3}\n'''.format(self.index, self.x, self.y, self.z)


class cFacelist:
    def __init__(self, object):
        global optionAllowMultiMats
        global matList
        global numMats
        global currentMatId

        self.facelist = []
        sgID = 0

        # Define smoothing groups (if enabled)
        if (collisionObject(object) == 0):
            if (optionSmoothingGroups):
                self.smoothing_groups = defineSmoothing(self, object)
            else:
                self.smoothing_groups = ''

        for face in object.data.polygons:
            if optionAllowMultiMats:
                if (collisionObject(object) < 2):
                    self.matid = matList.index(
                        object.material_slots[face.material_index].material.name)
                else:
                    self.matid = 0
            else:
                self.matid = currentMatId
            if (collisionObject(object) == 0):
                if (optionSmoothingGroups):
                    for group in self.smoothing_groups:
                        if group.count(face.index) == 0:
                            continue
                        else:
                            # TODO: Compress sg's
                            index = self.smoothing_groups.index(group)
                            sgID = index % 32

            temp = '''\t\t\t*MESH_FACE {0:4d}:    A: {1:4d} B: {2:4d} C: {3:4d} AB:    0 BC:    0 CA:    0\t *MESH_SMOOTHING {4}\t *MESH_MTLID {5}\n'''.format(
                face.index, face.vertices[0], face.vertices[1], face.vertices[2], sgID, self.matid)
            self.facelist.append(temp)

        if currentMatId < numMats - 1:
            currentMatId += 1
        else:
            currentMatId = 0

    def dump(self):
        temp = ''
        for x in self.facelist:
            temp = temp + str(x)
        return temp

    def __repr__(self):
        return '''{{\n{0}\t\t}}'''.format(self.dump())


class cUVdata:
    "Representation of mesh UV coordinates"

    def __init__(self, obj):
        self.uvdata = ''
        mesh = obj.data
        mesh.calc_loop_triangles()

        if (len(mesh.uv_layers) == 0) or (collisionObject(obj) > 0):
            self.uvdata = "\n\t\t*MESH_NUMTVERTEX 0"
        else:
            # For each UV layer
            for uv_layer_num, uv_layer in enumerate(mesh.uv_layers):
                tvlist = []
                tvdata = ''
                tfdata = ''
                # For each tri in the mesh
                for tri_num, tri in enumerate(mesh.loop_triangles):
                    tfdata += "\n\t\t\t*MESH_TFACE {0}".format(tri_num)
                    # For each index in the triangle loop (should be 3)
                    for vert_index in tri.loops:
                        # Look up corresponding UV data from the uv_layer array
                        uvvert = uv_layer.data[vert_index].uv
                        if uvvert not in tvlist:  # TODO bad performance
                            # only append vertices with unique uvs
                            tvlist.append(uvvert)
                            tvdata += ("\n\t\t\t*MESH_TVERT {0}\t{1}\t{2}\t{3}"
                                       ).format(len(tvlist)-1, aseFloat(uvvert[0]), aseFloat(uvvert[1]), aseFloat(0.0))
                        tfdata += "\t" + str(tvlist.index(uvvert))
                tvdata = ("\n\t\t*MESH_NUMTVERTEX " + str(len(tvlist)) +
                          "\n\t\t*MESH_TVERTLIST {" + tvdata + "\n\t\t}")
                tfdata = ("\n\t\t*MESH_NUMTVFACES " + str(len(mesh.loop_triangles)) +
                          "\n\t\t*MESH_TFACELIST {" + tfdata + "\n\t\t}")
                if uv_layer_num > 0:
                    tvdata = "\n\t\t*MESH_MAPPINGCHANNEL " + \
                        str(uv_layer_num + 1) + " {" + \
                        tvdata.replace("\n", "\n\t")
                    tfdata = tfdata.replace("\n", "\n\t") + "\n\t\t}"
                self.uvdata = self.uvdata + tvdata + tfdata

    def __repr__(self):
        return self.uvdata


class cCVertlist:
    "Representation of mesh vertex colour information"

    def __init__(self, obj):
        self.vertlist = []
        index = 0

        # Blender 2.63+
        bpy.ops.object.mode_set(mode='OBJECT')
        mesh = obj.data
        mesh.calc_loop_triangles()

        # Look up vertex colors for loop triangle indices
        for tri in mesh.loop_triangles:
            for vert_index in tri.loops:
                vcol = mesh.vertex_colors.active.data[vert_index].color
                self.vertlist.append(cCVert(index, vcol))
                index += 1

        self.length = len(self.vertlist)

    def dump(self):
        temp = ''
        for x in self.vertlist:
            temp = temp + str(x)
        return temp

    def __repr__(self):
        return '''\t\t*MESH_CVERTLIST {{\n{0}\t\t}}'''.format(self.dump())


class cCVert:
    def __init__(self, index, temp):
        self.index = index
        self.r = aseFloat(float(temp[0]))
        self.g = aseFloat(float(temp[1]))
        self.b = aseFloat(float(temp[2]))

    def __repr__(self):
        return '''\t\t\t*MESH_VERTCOL {0} {1} {2} {3}\n'''.format(self.index, self.r, self.g, self.b)


class cCFacelist:
    def __init__(self, facecount):
        temp = [0 for x in range(facecount)]
        self.facelist = []
        for index, data in enumerate(temp):
            self.facelist.append(cCFace(index, data))

    def dump(self):
        temp = ''
        for x in self.facelist:
            temp = temp + str(x)
        return temp

    def __repr__(self):
        return '''\t\t*MESH_CFACELIST {{\n{0}\t\t}}'''.format(self.dump())


class cCFace:
    def __init__(self, index, data):
        self.index = index
        self.vertices = []
        self.vertices.append(index * 3)
        self.vertices.append((index * 3) + 1)
        self.vertices.append((index * 3) + 2)

    def __repr__(self):
        return '''\t\t\t*MESH_CFACE {0} {1} {2} {3}\n'''.format(self.index, self.vertices[0], self.vertices[1], self.vertices[2])


class cNormallist:
    def __init__(self, object):
        self.normallist = []
        for face in object.data.polygons:
            self.normallist.append(cNormal(face, object))

    def dump(self):
        temp = ''
        for x in self.normallist:
            temp = temp + str(x)
        return temp

    def __repr__(self):
        return '''\t\t*MESH_NORMALS {{\n{0}\t\t}}'''.format(self.dump())


class cNormal:
    def __init__(self, face, object):
        self.faceindex = face.index
        self.facenormal = [aseFloat(x) for x in face.normal.to_tuple(4)]
        self.vertnormals = []
        object.data.calc_normals_split()
        for x in [object.data.loops[i] for i in face.loop_indices]:
            self.vertnormals.append(
                [str(x.vertex_index), [aseFloat(y) for y in x.normal]])

    def __repr__(self):
        return '''\t\t\t*MESH_FACENORMAL {0}\t{1}\t{2}\t{3}\n\t\t\t\t*MESH_VERTEXNORMAL {4}\t{5}\n\t\t\t\t*MESH_VERTEXNORMAL {6}\t{7}\n\t\t\t\t*MESH_VERTEXNORMAL {8}\t{9}\n'''.format(self.faceindex, self.facenormal[0], self.facenormal[1], self.facenormal[2], self.vertnormals[0][0], '\t'.join(self.vertnormals[0][1]), self.vertnormals[1][0], '\t'.join(self.vertnormals[1][1]), self.vertnormals[2][0], '\t'.join(self.vertnormals[2][1]))

# == Smoothing Groups and Helper Methods =================================


def defineSmoothing(self, object):
    print(object.name + ": Constructing Smoothing Groups")

    seam_edge_list = []
    sharp_edge_list = []

    _mode = bpy.context.active_object.mode
    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.mesh.select_all(action='DESELECT')
    setSelMode('EDGE')

    # Get seams and clear them
    bpy.ops.object.mode_set(mode='OBJECT')
    for edge in object.data.edges:
        if edge.use_seam:
            seam_edge_list.append(edge.index)
            edge.select = True

    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.mark_seam(clear=True)

    # Get sharp edges, convert them to seams
    bpy.ops.mesh.select_all(action='DESELECT')
    bpy.ops.object.mode_set(mode='OBJECT')
    for edge in object.data.edges:
        if edge.use_edge_sharp:
            sharp_edge_list.append(edge)
            edge.select = True

    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.mesh.mark_seam()

    bpy.ops.mesh.select_all(action='DESELECT')

    smoothing_groups = []
    face_list = []

    mode = getSelMode(self, False)
    setSelMode('FACE')

    for face in object.data.polygons:
        face_list.append(face.index)

    while len(face_list) > 0:
        bpy.ops.object.mode_set(mode='OBJECT')
        object.data.polygons[face_list[0]].select = True
        bpy.ops.object.mode_set(mode='EDIT')
        bpy.ops.mesh.select_linked(delimit={'SEAM'})

        # TODO - update when API is updated
        selected_faces = getSelectedFaces(self, True)
        smoothing_groups.append(selected_faces)
        for face_index in selected_faces:
            if face_list.count(face_index) > 0:
                face_list.remove(face_index)
        bpy.ops.mesh.select_all(action='DESELECT')

    setSelMode(mode, False)

    # Clear seams created by sharp edges
    bpy.ops.object.mode_set(mode='OBJECT')
    for edge in object.data.edges:
        if edge.use_seam:
            edge.select = True

    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.mesh.mark_seam(clear=True)

    bpy.ops.mesh.select_all(action='DESELECT')
    # Restore original uv seams
    bpy.ops.object.mode_set(mode='OBJECT')
    for edge_index in seam_edge_list:
        object.data.edges[edge_index].select = True

    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.mesh.mark_seam()

    print('\t' + str(len(smoothing_groups)) + ' smoothing groups found.')
    return smoothing_groups

# ===========================================================================
# // General Helpers
# ===========================================================================

# Check if the mesh is a collider and what kind
# 2 - skip materials, smoothing and uvs
# 1 - skip smoothing and uvs
# 0 - not a collision object

# motorsep hack 06-15-2023; disregard collision prefixes because same scene can be used to export to idTech4 and Unreal Engine, which doesn't care for materials on UCX_ (for example) meshes
def collisionObject(object):
    collisionPrefixes = ['UCX_', 'UBX_', 'USX_']
    for prefix in collisionPrefixes:
        if prefix in object.name:
            #return 2
            return 0
    collisionPrefixesAlt = ['collision_', 'shadow_']
    for prefix in collisionPrefixesAlt:
        if prefix in object.name:
            #return 1
            return 0 
    return 0

# Set the selection mode


def setSelMode(mode, default=True):
    if default:
        if mode == 'VERT':
            bpy.context.tool_settings.mesh_select_mode = [True, False, False]
        elif mode == 'EDGE':
            bpy.context.tool_settings.mesh_select_mode = [False, True, False]
        elif mode == 'FACE':
            bpy.context.tool_settings.mesh_select_mode = [False, False, True]
        else:
            return False
    else:
        bpy.context.tool_settings.mesh_select_mode = mode
        return True


def getSelMode(self, default=True):
    if default:
        if bpy.context.tool_settings.mesh_select_mode[0] == True:
            return 'VERT'
        elif bpy.context.tool_settings.mesh_select_mode[1] == True:
            return 'EDGE'
        elif bpy.context.tool_settings.mesh_select_mode[2] == True:
            return 'FACE'
        return False
    else:
        mode = []
        for value in bpy.context.tool_settings.mesh_select_mode:
            mode.append(value)

        return mode


def getSelectedFaces(self, index=False):
    selected_faces = []
    # Update mesh data
    bpy.ops.object.editmode_toggle()
    bpy.ops.object.editmode_toggle()

    _mode = bpy.context.active_object.mode
    bpy.ops.object.mode_set(mode='EDIT')

    object = bpy.context.active_object
    for face in object.data.polygons:
        if face.select == True:
            if index == False:
                selected_faces.append(face)
            else:
                selected_faces.append(face.index)

    bpy.ops.object.mode_set(mode=_mode)

    return selected_faces

class ASEExport(bpy.types.Operator, ExportHelper):
    """Main class implementing export to ASE."""

    bl_idname = "export_scene.ase"
    bl_label = "Export"
    filename_ext = ".ase"
    filter_glob: StringProperty(default="*.ase", options={'HIDDEN'})

    filepath: StringProperty(
        name="File Path",
        description="File path used for exporting the ASE file",
        maxlen=1024,
        default="")

    option_apply_stack: BoolProperty(
        name="Apply modifiers",
        description="Apply modifier stack before exporting",
        default=True)

    option_separate_by_material: BoolProperty(
        name="Separate by material",
        description="Separates objects by material",
        default=False)

    option_triangulate: BoolProperty(
        name="Triangulate",
        description="Triangulates all exportable objects",
        default=False)

    option_normals: BoolProperty(
        name="Recalculate Normals",
        description="Recalculate normals before exporting",
        default=False)

    option_remove_doubles: BoolProperty(
        name="Remove Doubles",
        description="Remove any duplicate vertices before exporting",
        default=False)

    option_apply_scale: BoolProperty(
        name="Scale",
        description="Apply scale transformation",
        default=True)

    option_apply_location: BoolProperty(
        name="Location",
        description="Apply location transformation",
        default=True)

    option_apply_rotation: BoolProperty(
        name="Rotation",
        description="Apply rotation transformation",
        default=True)

    option_smoothinggroups: BoolProperty(
        name="Smoothing Groups",
        description="Construct hard edge islands as smoothing groups",
        default=False)

    option_separate: BoolProperty(
        name="Separate",
        description="A separate ASE file for every selected object",
        default=False)

    option_submaterials: BoolProperty(
        name="Use Submaterials (UDK)",
        description="Export a single material with multiple sub materials",
        default=False)

    option_allowmultimats: BoolProperty(
        name="Allow Multiple Materials (UDK)",
        description="Allow multiple materials per geometry object",
        default=False)

    option_scale: FloatProperty(
        name="Scale",
        description="Object scaling factor (default: 1.0)",
        min=0.01,
        max=1000.0,
        soft_min=0.01,
        soft_max=1000.0,
        default=1.0)

    def draw(self, context):
        layout = self.layout

        box = layout.box()
        box.label(text='Essentials:')
        box.prop(self, 'option_apply_stack')
        box.prop(self, 'option_separate_by_material')
        box.prop(self, 'option_triangulate')
        box.prop(self, 'option_normals')
        box.prop(self, 'option_remove_doubles')
        box.label(text="Transformations:")
        box.prop(self, 'option_apply_scale')
        box.prop(self, 'option_apply_rotation')
        box.prop(self, 'option_apply_location')
        box.label(text="Materials:")
        box.prop(self, 'option_submaterials')
        box.prop(self, 'option_allowmultimats')
        box.label(text="Advanced:")
        box.prop(self, 'option_scale')
        box.prop(self, 'option_smoothinggroups')

    @classmethod
    def poll(cls, context):
        selected = context.selected_objects
        camera = context.scene.camera
        ok = selected or camera
        return ok

    def writeASE(self, filename, data):
        print('\nWriting', filename)
        try:
            file = open(filename, 'w')
        except IOError:
            print('Error: The file could not be written to. Aborting.')
        else:
            file.write(data)
            file.close()

    def execute(self, context):

        global optionScale
        global optionSubmaterials
        global optionSmoothingGroups
        global optionAllowMultiMats

        global aseHeader
        global aseScene
        global aseMaterials
        global aseGeometry

        global currentMatId
        global numMats
        global matList

        # Set globals and reinitialize ase components
        aseHeader = ''
        aseScene = ''
        aseMaterials = ''
        aseGeometry = ''

        optionScale = self.option_scale
        optionSubmaterials = self.option_submaterials
        optionSmoothingGroups = self.option_smoothinggroups
        optionAllowMultiMats = self.option_allowmultimats

        matList = []
        currentMatId = 0
        numMats = 0

        # First ensure we are in Object mode otherwise functions like select_all
        # will fail
        bpy.ops.object.mode_set(mode='OBJECT')

        # Build ASE Header, Scene
        print('\nAscii Scene Export by The Dark Mod team\n')
        print('Objects selected: ' + str(len(bpy.context.selected_objects)))
        aseHeader = str(cHeader())
        aseScene = str(cScene())

        # Back up duplicates, work on originals
        objects = []
        bup_objs = []
        bup_obj_names = {}
        bup_mesh_names = {}
        for object in bpy.context.selected_objects:
            if object.type == 'MESH':
                bpy.ops.object.select_all(action='DESELECT')
                bpy.context.view_layer.objects.active = object
                objects.append(object)
                object.select_set(state=True)
                bpy.ops.object.duplicate()
                dup = bpy.context.active_object
                dup.name = "temp"
                dup.data.name = "temp"
                bup_objs.append(dup)
                bup_obj_names[dup] = object.name
                bup_mesh_names[dup] = object.data.name
            # Apply modifiers
            if self.option_apply_stack:
                bpy.context.view_layer.objects.active = object
                for mod in 	object.modifiers:                           
                        bpy.ops.object.modifier_apply(modifier=mod.name)

                object.modifiers.clear()

        # Separate by material
        bpy.ops.object.select_all(action='DESELECT')
        if self.option_separate_by_material:
            for object in objects:
                bpy.context.view_layer.objects.active = object
                object.select_set(state=True)
                bpy.ops.object.mode_set(mode='EDIT')
                bpy.ops.mesh.separate(type='MATERIAL')
            for object in bpy.context.selected_objects:
                if object.type == 'MESH':
                    if object not in objects:
                        objects.append(object)

        objects.sort(key=lambda a: a.name)

        aseMaterials = str(ASEMaterials(objects))

        for object in objects:
            bpy.context.view_layer.objects.active = object
            object.select_set(state=True)

            # Apply options
            bpy.ops.object.mode_set(mode='EDIT')
            if self.option_remove_doubles:
                bpy.ops.object.mode_set(mode='EDIT')
                bpy.ops.mesh.select_all(action='SELECT')
                bpy.ops.mesh.remove_doubles()
            if self.option_triangulate:
                bpy.ops.object.mode_set(mode='EDIT')
                bpy.ops.mesh.select_all(action='SELECT')
                bpy.ops.mesh.quads_convert_to_tris()
            if self.option_normals:
                bpy.ops.object.mode_set(mode='EDIT')
                bpy.ops.mesh.select_all(action='SELECT')
                bpy.ops.mesh.normals_make_consistent()

            # Transformations
            bpy.ops.object.mode_set(mode='OBJECT')
            bpy.ops.object.transform_apply(location=self.option_apply_location,
                                           rotation=self.option_apply_rotation, scale=self.option_apply_scale)

            # Construct ASE Geometry Nodes
            aseGeometry += str(cGeomObject(object))

        # Clean up
        bpy.ops.object.mode_set(mode='OBJECT')
        bpy.ops.object.select_all(action='DESELECT')
        for object in objects:
            bpy.context.view_layer.objects.active = object
            object.select_set(state=True)
            bpy.ops.object.delete()

        # Restore scene from duplicates
        for object in bup_objs:
            object.name = bup_obj_names[object]
            object.data.name = bup_mesh_names[object]
            bpy.context.view_layer.objects.active = object
            object.select_set(state=True)

        aseModel = ''
        aseModel += aseHeader
        aseModel += aseScene
        aseModel += aseMaterials
        aseModel += aseGeometry

        # Write the ASE file
        self.writeASE(self.filepath, aseModel)

        return {'FINISHED'}

# Blender registration

def lwo_menu_func(self, context):
    self.layout.operator(LWOExport.bl_idname, text="Lightwave Object (.lwo)")

def ase_menu_func(self, context):
    self.layout.operator(ASEExport.bl_idname, text="ASCII Scene (.ase)")

def register():
    bpy.utils.register_class(ASEExport)
    bpy.types.TOPBAR_MT_file_export.append(ase_menu_func)
    bpy.utils.register_class(LWOExport)
    bpy.types.TOPBAR_MT_file_export.append(lwo_menu_func)

def unregister():
    bpy.utils.unregister_class(ASEExport)
    bpy.types.TOPBAR_MT_file_export.remove(ase_menu_func)
    bpy.utils.unregister_class(LWOExport)
    bpy.types.TOPBAR_MT_file_export.remove(lwo_menu_func)