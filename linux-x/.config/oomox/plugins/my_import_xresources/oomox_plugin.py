import os
import subprocess
import importlib.util
import importlib.machinery
import sys

from oomox_gui.plugin_api import OomoxImportPlugin
from oomox_gui.config import DEFAULT_ENCODING
from oomox_gui.theme_model import get_theme_model
from oomox_gui.i18n import translate


PLUGIN_DIR = os.path.dirname(os.path.realpath(__file__))


class SourceFileLoaderWithoutCache(importlib.machinery.SourceFileLoader):
    def _cache_bytecode(self, _source_path, _bytecode_path, data):
        pass


class XrdbCache():

    _cache = None

    @classmethod
    def get(cls):
        if cls._cache:
            return cls._cache

        timeout = 10
        command = ['xrdb', '-query']

        result = {}
        with subprocess.Popen(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT
        ) as proc:
            for line in iter(proc.stdout.readline, b''):
                line = line.decode(DEFAULT_ENCODING)
                key, value, *_rest = line.split(':')
                key = key.lstrip('*').lstrip('.')
                value = value.strip()
                result[key] = value
            proc.communicate(timeout=timeout)
            if proc.returncode == 0:
                cls._cache = result
                return result
        print('xrdb not found')
        return None

    @classmethod
    def clear(cls):
        cls._cache = None


class Plugin(OomoxImportPlugin):

    name = 'my_import_xresources'
    display_name = 'MyXresources'
    plugin_theme_dir = os.path.abspath(
        os.path.join(PLUGIN_DIR, 'colors')
    )

    theme_model_import = [
        {
            'key': 'MYX_THEME_COLOR',
            'type': 'int',
            'fallback_value': 4,
            'min_value': 0,
            'max_value': 15,
            'display_name': translate('Theme color'),
            'reload_theme': True,
        }
    ]

    def read_colorscheme_from_path(self, preset_path):

        if not preset_path.startswith(self.plugin_theme_dir + '/'):
            print('Preset path {preset_path} is not under {plugin_theme_dir}, '
                  'returning empty color scheme'.format(
                      preset_path = preset_path,
                      plugin_theme_dir = self.plugin_theme_dir))
            return {}
        preset_name = preset_path[len(self.plugin_theme_dir) + 1:]
        preset_module_name = __name__ + '.' + preset_name
        preset_module = sys.modules.get(preset_module_name)
        if not preset_module:
            preset_module_spec = importlib.util.spec_from_file_location(
                preset_module_name,
                preset_path,
                loader=SourceFileLoaderWithoutCache(preset_module_name, preset_path),
                submodule_search_locations=None)
            preset_module = importlib.util.module_from_spec(preset_module_spec)
            preset_module_spec.loader.exec_module(preset_module) 
            sys.modules[preset_module_name] = preset_module

        theme_settings = {}
        for section in get_theme_model().values():
            for item in section:
                if 'key' in item and item['key'] not in theme_settings:
                    theme_settings[item['key']] = item.get('fallback_value')

        color_scheme = {}
        for key, value in preset_module.get_theme_from_xrdb(
                dict(theme_settings), XrdbCache.get()):
            if key not in theme_settings:
                continue
            color_scheme[key] = value 
        XrdbCache.clear()
        return color_scheme
