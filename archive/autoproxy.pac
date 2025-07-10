function FindProxyForURL(url, host)
{
	return host !== "127.0.0.1" ? "SOCKS5 127.0.0.1:12345" : "DIRECT";
}

