tenable-securitycenter
======================

[Tenable SecurityCenter REST API](https://support.tenable.com/support-center/cerberus-support-center/includes/widgets/sc_api/index.html) client

Usage
-----
Use Network.Tenable.SecurityCenter.Token to authenticate and obtain a Network.Tenable.SecurityCenter.Types.Token and CookieJar session.

Use the Token and CookieJar session when calling other APIs like Asset
using Network.Tenable.SecurityCenter.Asset.

Automating Host Discovery
-------------------------
Dynamic host discovery with pings, SYNs, etc. can be costly and time-consuming. Far better to automate this process by getting hosts from cloud orchestration systems like vCenter/vCloud, OpenStack, Joyent Triton, or AWS.

The executable this project builds is designed to be UNIX pipeline friendly.

### Example Usage

Create config.json:

```json
{
    "host": "securitycenter.example.com",
    "username": "limited-apiuser",
    "password": "limited-apiusers-strong-generated-password"
}
```

Ideally that password wouldn't be on disk for lots of reasons, so make sure to protect config.json with volume encryption and file system permissions. Don't store it on an untrusted system. I'm contemplating better solutions to that problem including Vault or encrypted DynamoDB, but they don't belong in this project directly. If you have ideas about this I'm all ears.

Run this pipeline to pull all private IPs for every EC2 instance in your AWS research account and load them into asset ID 1234 (to determine the asset ID from SecurityCenter web UI visit an asset and check the number at the end of the URL):

```bash
aws --profile research ec2 describe-instances |
jq '.Reservations[].Instances[].NetworkInterfaces[].PrivateIpAddress' --raw-output | sort | uniq | stack exec tenable-securitycenter-exe config.json 1234
```

There will be no output, but a successful run will have an exit code of 0.
