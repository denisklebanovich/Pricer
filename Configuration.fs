module Configuration

type ConfigKey = string
type ConfigValue = string
type ConfigEntry = { Key : ConfigKey; Value : ConfigValue }
type ConfigCategory = { Category : string; Config : ConfigEntry[]  }
type JsonConfig = ConfigCategory []

type ConfigurationRecord = 
    {
        Key : ConfigKey
        Value : ConfigValue
    }

type Configuration = Map<ConfigKey, ConfigValue>
type MarketData = Map<ConfigKey, ConfigValue>

let defaultMarketData =
    [ 
        "FX::USDPLN","3.76"
        "FX::PLNUSD","0.3"
        "FX::USDEUR","0.95"
        "FX::EURGBP","0.9"
    ] |> Map.ofList