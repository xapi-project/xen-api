components = ["xapi", "datamodel", "common", "client", "xenstored", "v6d", "block_device_io"];
component_modules = {}

function fill_components()
{
	for (i in components)
		component_modules[components[i]] = eval('modules_' + components[i]);
}
