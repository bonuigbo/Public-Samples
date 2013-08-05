using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace neural_netProgram
{
    class StatsTable
    {
        private Dictionary<string, double> countries;
        private Dictionary<string, double> businesses;

        public StatsTable()
        {
            countries = new Dictionary<string, double>();
            businesses = new Dictionary<string, double>();
        }

        public Dictionary<string, double> Countries
        {
            get { return countries; }
        }

        public Dictionary<string, double> Businesses
        {
            get { return businesses; }
        }

        public List<string[]> CountriesTable
        {
            get
            {
                List<string[]> table = new List<string[]>();
                
                foreach (KeyValuePair<string, double> value in countries)
                {
                    List<string> dataLine = new List<string>();
                    dataLine.Add(value.Key);
                    dataLine.Add(value.Value.ToString());
                    table.Add(dataLine.ToArray());
                }
                return table;
            }
        }

        public List<string[]> BusinessesTable
        {
            get
            {
                List<string[]> table = new List<string[]>();
                foreach (KeyValuePair<string, double> value in businesses)
                {
                    table.Add(new string[] { value.Key, value.Value.ToString() });
                }
                return table;
            }
        }
    }
    /*
     *  For reading the design data file, and parsing out
     *  the statistics for businesses, and countries
     */
    class Interpreter
    {
        public StatsTable buyers;
        public StatsTable nonbuyers;
        public StatsTable scaledScores;
        public List<string[]> rawData;
        

        public Interpreter(List<string[]> rawData)
        {
            buyers = new StatsTable();
            nonbuyers = new StatsTable();
            scaledScores = new StatsTable();
            this.rawData = rawData;
            ParseData();
            SetScaledScores();
        }

        private void ParseData()
        {
            int count = -1;
            foreach (string[] row in rawData)
            {
                count++;
                //skip headers
                if (count == 0)
                    continue;
                string country = row[1];
                string business = row[2];
                if (row[0].ToLower().Contains("not"))
                {
                    if (!nonbuyers.Businesses.ContainsKey(business))
                        nonbuyers.Businesses[business] = 0.0;
                    if (!nonbuyers.Countries.ContainsKey(country))
                        nonbuyers.Countries[country] = 0.0;
                    nonbuyers.Countries[country] += 1.0;
                    nonbuyers.Businesses[business] += 1.0;
                }
                else
                {
                    if (!buyers.Businesses.ContainsKey(business))
                        buyers.Businesses[business] = 0.0;
                    if (!buyers.Countries.ContainsKey(country))
                        buyers.Countries[country] = 0.0;
                    buyers.Countries[country] += 1.0;
                    buyers.Businesses[business] += 1.0;
                }

            }
        }

        private void SetScaledScores()
        {
            SetBusinessValues();
            SetCountryValues();
        }

        private void SetBusinessValues()
        {
            foreach (KeyValuePair<string, double> buyvalues in buyers.Businesses)
            {
                string buybusiness = buyvalues.Key;
                double buycount = buyvalues.Value;
                string normalized_buy = buybusiness.Trim().ToLower();
                bool hasCounterpart = false;
                foreach (KeyValuePair<string, double> nonvalues in nonbuyers.Businesses)
                {
                    string nonbusiness = nonvalues.Key;
                    double noncount = nonvalues.Value;
                    string normalized_non = nonbusiness.Trim().ToLower();
                    if (!scaledScores.Businesses.ContainsKey(normalized_non))
                        scaledScores.Businesses[normalized_non] = -1.0 * noncount;
                    if (String.Equals(normalized_non, normalized_buy))
                    {
                        hasCounterpart = true;
                        if (buycount > noncount)
                            scaledScores.Businesses[normalized_non] = buycount / noncount;
                        else
                            scaledScores.Businesses[normalized_non] = -1.0 * noncount / buycount;
                    }
                }
                if (!hasCounterpart)
                    scaledScores.Businesses[normalized_buy] = buycount;
            }
        }

        private void SetCountryValues()
        {
            foreach (KeyValuePair<string, double> buyvalues in buyers.Countries)
            {
                string buycountry = buyvalues.Key;
                double buycount = buyvalues.Value;
                string normalized_buy = buycountry.Trim().ToLower();
                bool hasCounterpart = false;
                foreach (KeyValuePair<string, double> nonvalues in nonbuyers.Countries)
                {
                    string noncountry = nonvalues.Key;
                    double noncount = nonvalues.Value;
                    string normalized_non = noncountry.Trim().ToLower();
                    if (!scaledScores.Countries.ContainsKey(normalized_non))
                        scaledScores.Countries[normalized_non] = -1.0 * noncount;
                    if (String.Equals(normalized_non, normalized_buy))
                    {
                        hasCounterpart = true;
                        if (buycount > noncount)
                            scaledScores.Countries[normalized_non] = buycount / noncount;
                        else
                            scaledScores.Countries[normalized_non] = -1.0 * noncount / buycount;
                    }
                }
                if (!hasCounterpart)
                    scaledScores.Countries[normalized_buy] = buycount;
            }
        }
    }

}
