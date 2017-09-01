using System;
using System.Linq;
using XenAPI;


namespace GetVmRecords
{
    static class GetVariousRecords
    {
        public static void Run(Session session)
        {
            PrintTitle("Printing various records");
            PrintHostRecords(session);
            PrintStorageRepositories(session);
            PrintVmRecords(session);
            PrintPhysicalNetworkInterfaces(session);
        }

        private static void PrintHostRecords(Session session)
        {
            PrintTitle("Hosts");
            var hostRecords = Host.get_all_records(session);

            foreach (var hostRec in hostRecords)
            {
                var host = hostRec.Value;
                System.Console.WriteLine("Name: {0}", host.name_label);
                System.Console.WriteLine("Hostname: {0}", host.hostname);
                System.Console.WriteLine("Description: {0}", host.name_description);
                System.Console.WriteLine();
            }
        }

        private static void PrintStorageRepositories(Session session)
        {
           PrintTitle("Storage Repositories");
           var srRecords = SR.get_all_records(session);

           foreach (var srRec in srRecords)
            {
                var sr = srRec.Value;
                System.Console.WriteLine("Name: {0}", sr.name_label);
                System.Console.WriteLine("Description: {0}", sr.name_description);
                System.Console.WriteLine("Usage: {0:0.0}GB / {1:0.0}GB", sr.physical_utilisation / 1e9, sr.physical_size / 1e9);
                System.Console.WriteLine();
            }
        }

        private static void PrintVmRecords(Session session)
        {
            PrintTitle("Virtual Machines");

            var vmRecords = VM.get_all_records(session);
            foreach (var vmRec in vmRecords)
            {
                var vm = vmRec.Value;
                System.Console.WriteLine(vm.is_a_template ? "VM name: {0}" : "Template name {0}", vm.name_label);
                System.Console.WriteLine("Power state: {0}", vm.power_state);
                string ops = string.Join(",", vm.allowed_operations.Select(op => op.ToString()));
                System.Console.WriteLine("Allowed operations: {0}", ops);
                System.Console.WriteLine("vCPUs: {0}", vm.VCPUs_at_startup);
                System.Console.WriteLine();
            }
        }

        private static void PrintPhysicalNetworkInterfaces(Session session)
        {
            PrintTitle("Physical network interfaces");
            var pifRecords = PIF.get_all_records(session);

            foreach (var pifRec in pifRecords)
            {
                var pif = pifRec.Value;

                Host host = Host.get_record(session, pif.host);
                System.Console.WriteLine("Host: {0}", host.name_label);
                System.Console.WriteLine("IP: {0}", pif.IP);
                System.Console.WriteLine("MAC address: {0}", pif.MAC);
                System.Console.WriteLine();
            }
        }

        private static void PrintTitle(string title)
        {
            System.Console.WriteLine();
            System.Console.WriteLine("*** {0} ***", title);
            System.Console.WriteLine();
        }
    }
}
