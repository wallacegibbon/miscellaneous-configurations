function FindProxyForURL(url, host) {
	if (host !== "127.0.0.1")
		return "SOCKS5 127.0.0.1:12345";
	else
		return "DIRECT";
}
