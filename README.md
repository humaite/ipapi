# ipapi

An OTP application

## Build

```sh
rebar3 compile
```

## Usage (shell)

This application requires `IPAPI_API_KEY` environment variable to be
start correctly.

```sh
export IPAPI_API_KEY=private_key
rebar3 shell
```

## Usage (dev)

```erlang
{deps, [ipapi]}.
```

## References and Resources

[Official IP-Addres-API repository](https://github.com/NikolaiT/IP-Address-API)

[ipapi.is](https://ipapi.is/)
