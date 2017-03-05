tenable-securitycenter
======================

[Tenable SecurityCenter REST API](https://support.tenable.com/support-center/cerberus-support-center/includes/widgets/sc_api/index.html) client

Usage
-----
Use Network.Tenable.SecurityCenter.Token to authenticate and obtain a Network.Tenable.SecurityCenter.Types.Token and CookieJar session.

Use the Token and CookieJar session when calling other APIs like Asset
using Network.Tenable.SecurityCenter.Asset.

```plain
Tenable SecurityCenter Host: securitycenter.example.com
Username: limited-apiuser
Password: limited-apiusers-strong-generated-password
Just (GetAssetByIdResponse {assetByIdId = "1234", assetByIdStaticTypeFields = StaticTypeFields {typeFieldsDefinedIPs = "10.0.0.0/8,172.16.0.0/12,192.168.0.0/16"}})
["10.0.0.0/8","172.16.0.0/12","192.168.0.0/16"]
Just (GetAssetByIdResponse {assetByIdId = "1234", assetByIdStaticTypeFields = StaticTypeFields {typeFieldsDefinedIPs = "10.0.0.0/8,172.16.0.0/12,192.168.0.0/16"}})
```
