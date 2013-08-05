using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace NeuralNet
{
    /// <summary>
    /// A class that can be used to save and print logs, and can be activated and
    /// deactivated
    /// </summary>
    public class Logger
    {
        private List<string> logs;
        private bool debug;

        public Logger(bool debug)
        {
            this.debug = debug;
            logs = new List<string>();
        }

        public Logger()
        {
            this.debug = true;
            logs = new List<string>();
        }

        public void Log(string log)
        {
            if (debug)
                logs.Add(log);
        }

        public void PrintLogs()
        {
            using (StreamWriter writer = new StreamWriter(new FileStream(Configuration.log,
                        FileMode.Create,
                        FileAccess.Write)))
            {
                foreach (var logString in logs)
                {
                    writer.WriteLine(logString);
                }
            }
            logs.Clear();
        }

        public bool Debug
        {
            get { return debug; }
            set { debug = value; }
        }

    }
}
