{
	'target_defaults': {
		'configurations': {
			'Debug': {
				'msvs_configuration_platform': 'x64'
			}
		},
		'msbuild_toolset': 'v120_xp',
	},
	'targets':[{
		'target_name': 'winexpect',
		'type':'executable',
		'sources': ['winexpect.c'],
		'msbuild_settings': {
			'ClCompile': {
				'RuntimeLibrary': 'MultiThreaded'
			},
			'Link': {	
				'SubSystem': 'Console'
			},
		}
	}]
}
