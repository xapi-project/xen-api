executables = ["xapi", "xenstored", "v6d", "block_device_io", "db_actions", "xe"];
libraries = ["datamodel", "common", "client", "sexpr"];

components = executables.concat(libraries);
component_modules = {}

function fill_components()
{
	for (i in components)
		component_modules[components[i]] = eval('modules_' + components[i]);
}
