# ipapi

A quick and dirty implementation of [ipapi.is](https://ipapi.is/)
service using [gun](https://ninenines.eu/docs/en/gun),
[certifi](https://github.com/certifi/erlang-certifi) and
[jsone](https://github.com/sile/jsone) in pure Erlang.

## Build

```sh
rebar3 compile
```

## Usage (shell)

This application requires `IPAPI_API_KEY` environment variable to be
start correctly.

```sh
export IPAPI_API_KEY="private_key"
rebar3 shell
```

```erlang
ipapi:simple("1.2.3.4").
{ok,#{<<"abuse">> =>
          #{<<"address">> => <<"6 Cordelia Street, South Brisbane">>,
            <<"email">> => <<"helpdesk@apnic.net">>,
            <<"name">> => <<"Resource Quality Assurance">>,
            <<"phone">> => <<>>},
      <<"company">> =>
          #{<<"abuser_score">> => <<"0.0039 (Low)">>,
            <<"name">> => <<"Resource Quality Assurance">>,
            <<"network">> => <<"1.2.3.0 - 1.2.3.255">>,
            <<"type">> => <<"business">>,
            <<"whois">> => <<"https://api.ipapi.is/?whois=1.2.3.0">>},
      <<"elapsed_ms">> => 1.61,<<"ip">> => <<"1.2.3.4">>,
      <<"is_abuser">> => true,<<"is_bogon">> => false,
      <<"is_crawler">> => false,<<"is_datacenter">> => false,
      <<"is_mobile">> => false,<<"is_proxy">> => false,
      <<"is_tor">> => false,<<"is_vpn">> => false,
      <<"location">> =>
          #{<<"city">> => <<"Brisbane">>,<<"continent">> => <<"OC">>,
            <<"country">> => <<"Australia">>,
            <<"country_code">> => <<"AU">>,<<"is_dst">> => false,
            <<"latitude">> => -27.46794,
            <<"local_time">> => <<"2024-11-08T19:30:00+10:00">>,
            <<"local_time_unix">> => 1731058200,
            <<"longitude">> => 153.02809,
            <<"state">> => <<"Queensland">>,
            <<"timezone">> => <<"Australia/Brisbane">>,
            <<"zip">> => <<"4000">>},
      <<"rir">> => <<"APNIC">>}}
```

## Usage (dev)

```erlang
{deps, [ipapi]}.
```

## References and Resources

[Official IP-Addres-API repository](https://github.com/NikolaiT/IP-Address-API)

[ipapi.is](https://ipapi.is/)
