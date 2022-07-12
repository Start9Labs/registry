# Lightning Network Daemon (LND)

## Dependencies

LND on the Embassy requires a full archival Bitcoin node to function. Since your Embassy Bitcoin node is pruned by default, an additional service, Bitcoin Proxy, is also required.

## LND Config

Your LND node is highly configurable. Many settings are considered _advanced_ and should be used with caution. For the vast majority of users and use-cases, we recommend using the defaults. Once configured, you may start your node!

## Bitcoin Proxy Config

On the LND page, scroll down to find the Bitcoin Proxy dependency. Click `Configure`. This will automatically configure Bitcoin Proxy to satisfy LND.

# Lightning Usage Guide

## Using a Wallet

Enter your LND-Connect QR code (located in `properties`) into any wallet that supports connecting to a remote LND node over Tor. For a list of compatible wallets, see <a href="https://github.com/start9labs/lnd-wrapper/blob/master/docs/wallets.md" target="_blank">https://github.com/start9labs/lnd-wrapper/blob/master/docs/wallets.md</a>.

## Getting On-Chain Funds

Before you can open and channel and start transacting on the Lightning network, you need some Bitcoin stored on your LND node. Be advised, Bitcoin funds that you transfer to your LND node are hot, meaning, the are stored directly on your Embassy. There is no way to use cold storage when using Lightning, which is why people call it reckless. For this reason, it is usually unwise to move large amounts of Bitcoin to your LND node. That said, you don't want to move a tiny amount either, since that would limit your purchasing power on the Lightning network. We recommend moving about 500,000-5,000,000 satoshis, or .005-.05 Bitcoin, which at current (May 7, 2021) prices is about $250-$2,500. This gives you a solid amount of purchasing power, but hopefully wouldn't ruin your life if something were to go terribly wrong. If you feel comfortable using more Bitcoin, then by all means, go for it.

## Depositing to LND

When using LND or any wallet that is connected to LND it is important to note that until "Synced to Chain" in the Properties page is reporting ✅, your deposits to your LND on-chain wallet may not appear.

## Opening a Channel and Getting Outbound Liquidity

Once your LND node is synced, it's time to open a channel. Opening a channel with a well-connected node is how you get connected to the rest of the network, and it immediately grants you outbound liquidity. Meaning, you will be able to send money to others. Unless you are planning to become a Lightning Service Provider, you do not want to open more than a couple of channels at most. Managing many channels is difficult, it can be quite expensive, and unless you plan to devote significant resources in the form of time and Bitcoin, there is no profit in it. If your goal is to use Lightning to benefit from instant and near-free transactions, you only need 2-3 good channels.

If you are looking for destinations for your first channel, we suggest you open a channel with the [Start9 HQ](025d28dc4c4f5ce4194c31c3109129cd741fafc1ff2f6ea53f97de2f58877b2295@64.225.19.231:9735) node, which is already very well connected.

It is not recommended to open a channel less than 100,000 satoshi, or .001 BTC, or $50 USD in today's prices. Anything less, and it's possible that the cost to open and close the channel might approach the size of the channel itself. The bigger the channel you open, the more outbound liquidity you will have, which means you have more spending power on the network. In this tutorial, we are going to open a channel of 2,000,000 satoshi. When opening a channel with Start9 HQ, we ask that you make it a private channel, meaning it will not display publicly on network graph. The reason for this is that unless you plan to be a very active Lightning Node Operator, having public channels decreases not only the reliability of your node but also hurts Start9's ability to route payments for you. If you do intend to be a serious node operator, we require that your channel be for a minimum of 5,000,000 sats. Please contact us in one of our community channels for further details.

## Getting Inbound Liquidity

If you want to receive payments, you will need some inbound liquidity.

The first, easiest, and best way to get inbound liquidity is to use your outbound liquidity to buy something. Any Bitcoin you spend using your outbound liquidity is Bitcoin you can now receive back. So if there is something you want to buy, like a Start9 Embassy or a t-shirt from the Start9 store, simply make the purchase, and you will then have inbound liquidity equal to the amount of Satoshis you spend.

Option 2 is to personally ask Start9 for an invoice for however much inbound liquity you want. Then you send Bitcoin to the invoice, and in turn we will transfer fiat currency to you equal to the amount of the Bitcoin you send us. In other words, Start9 will buy some Bitcoin from you at market rate, such that you then have inbound liquidity. In either case, you are spending or selling some Bitcoin.

The only way to get inbound liquidity without spending or selling Bitcoin is to convince someone to open a channel with you, just as you opened a channel with Start9 HQ. This may be a difficult task, since there is not much incentive for someone to open a channel with you unless you are also very well connected. Also, you will need to make sure that they too, are well connected with plenty of inbound liquidity, or else your inbound liquidity with them will not really matter. In other words, they might be the only person capable of paying you.

So options 1 or 2 are best. Use your Lightning node's outbound liquidity to either purchase something or sell some Bitcoin. Now, you can pay and get paid using Lightning in an amount equal to your outbound and inbound liquidity.

## Sending payments over Lightning

Once you have open channels and are ready to transact on the Lightning Network, it is important to note that until "Synced to Graph" in the Properties page is reporting ✅, you may experience problems finding routes to your destination.

