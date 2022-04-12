/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.intranet;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadbandIntranetConnectivityTest  extends AutomaticsTestBase {
	
    /**
     * Verify the Ping operation is successful between 2.4 GHz Private Wi-Fi and Ethernet client
     * <ol>
     * <li>Verify the 2.4 GHz Private Wi-Fi SSID is enabled using WebPA</li>
     * <li>Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify connection status</li>
     * <li>Verify the IPv4 Address is retrieved from the client connected to 2.4GHz Private Wi-Fi Network</li>
     * <li>Verify the IPv6 Address is retrieved from the client connected to 2.4GHz Private Wi-Fi Network</li>
     * <li>Verify the internet is accessible in the client connected to 2.4 GHz Private Wi-Fi Network</li>
     * <li>Verify the client 2 connected to ethernet has IPv4 Address assigned from DHCP</li>
     * <li>Verify the IPv6 Address is retrieved from the client connected to Ethernet</li>
     * <li>Verify the internet is accessible in the client connected to Ethernet</li>
     * <li>Verify the Ping Connection for IPv4 Address is successful from 2.4GHz Wi-Fi client to Ethernet Client</li>
     * <li>Verify the Ping Connection for IPv6 Address is successful from 2.4GHz Wi-Fi client to Ethernet Client</li>
     * <li>Verify the Ping Connection for IPv4 Address is successful from Ethernet client to 2.4GHz Wi-Fi client</li>
     * <li>Verify the Ping Connection for IPv6 Address is successful from Ethernet client to 2.4GHz Wi-Fi client</li>
     * </ol>
     * @author SATHYA KISHORE
     * @refactor Athira
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-INTRANET-ACC-1001")
    public void testToVerifyIntranetConnectivityBetween2GhzAndEthernetClientUsingPing(Dut device) {

	String testId = "TC-RDKB-INTRANET-ACC-101";
	String testStepNumber = "s1";
	String errorMessage = null; // stores error message
	boolean status = false; // stores test steps status
	Dut connectedClient2GHzDut = null; // connected device to be verified
	Dut connectedClientEthernetDut = null; // connected device to be verified
	BroadBandResultObject result = null; // stores test result and error message

	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-INTRANET-ACC-1001");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify the Ping operation is successful between 2.4 GHz and Ethernet clients");

	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1. Verify the 2.4 GHz Private Wi-Fi SSID is enabled using WebPA");
	    LOGGER.info("2. Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify connection status ");
	    LOGGER.info(
		    "3. Verify the IPv4 Address is retrieved  from the client connected to 2.4GHz Private Wi-Fi Network");
	    LOGGER.info(
		    "4. Verify the IPv6 Address is retrieved  from the client connected to 2.4GHz Private Wi-Fi Network");
	    LOGGER.info(
		    "5. Verify the internet is accessible in the client connected to 2.4 GHz Private Wi-Fi Network");
	    LOGGER.info("6. Verify the client 2 connected to ethernet has IPv4 Address assigned from DHCP");
	    LOGGER.info("7. Verify the IPv6 Address is retrieved  from the client connected to Ethernet");
	    LOGGER.info("8. Verify the internet is accessible in the client connected to Ethernet");
	    LOGGER.info(
		    "9. Verify the Ping Connection for IPv4 Address is successful from 2.4GHz Wi-Fi client to Ethernet Client");
	    LOGGER.info(
		    "10. Verify the Ping Connection for IPv6 Address is successful from 2.4GHz Wi-Fi client to Ethernet Client");
	    LOGGER.info(
		    "11. Verify the Ping Connection  for IPv4 Address is successful from Ethernet client to 2.4GHz Wi-Fi client");
	    LOGGER.info(
		    "12. Verify the Ping Connection  for IPv6 Address is successful from Ethernet client to 2.4GHz Wi-Fi client");

	    LOGGER.info("#######################################################################################");

	    /**
	     * Step 1: Verify the 2.4 GHz Private Wi-Fi SSID is enabled using WebPA
	     *
	     */
	    testStepNumber = "s1";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 1: Verify the 2.4 GHz Private Wi-Fi SSID is enabled using WebPA");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the command to enable 2.4 GHz Private Wi-Fi SSID using parameter Device.WiFi.SSID.10001.Enable");
	    LOGGER.info("STEP 1: EXPECTED: 2.4 GHz private Wi-Fi radio should be enabled");
	    status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, true);
	    errorMessage = "Enabling 2.4 GHz Private Wi-Fi SSID via WebPA failed";
	    if (status) {
		LOGGER.info("S1 ACTUAL: 2.4 GHz Private Wi-Fi SSID is enabled successfully using WebPA");
	    } else {
		LOGGER.error("S1 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 2: Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify connection status
	     *
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 2: Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify connection status");
	    LOGGER.info(
		    "STEP 2: ACTION : Connect to 2.4 GHz wifi using below commands Linux :nmcli dev wifi connect <ssid> password <passwd> Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
	    LOGGER.info("STEP 2: EXPECTED: Device should be connected with 2.4 GHz wifi network");
	    errorMessage = "Unable to connect to 2.4 GHz private Wi-Fi Network Or 2.4 GHz WiFi capable devices are not available";
	    try {
		connectedClient2GHzDut = BroadBandConnectedClientUtils
			.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = null != connectedClient2GHzDut;
	    if (status) {
		LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		LOGGER.info("S2 ACTUAL: Device has been connected with 2.4 GHz private Wi-Fi network");
	    } else {
		LOGGER.error("S2 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 3: Verify the IPv4 Address is retrieved from the client connected to 2.4GHz Private Wi-Fi Network
	     *
	     */
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 3: Verify the IPv4 Address is retrieved  from the client connected to 2.4GHz Private Wi-Fi Network");
	    LOGGER.info(
		    "STEP 3: ACTION : Get the device IPv4 address using below commandLinux : ifconfig wlan0 |grep -i \"inet addr:\"Windows: ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"IPv4 Address\"");
	    LOGGER.info(
		    "STEP 3: EXPECTED: Connected Wireless client should be assigned with the IP Address between DHCP Range");
	    String ipv4AddressRetrievedFrom2GHzClient = BroadBandConnectedClientUtils
		    .getIpv4AddressFromConnClient(tapEnv, device, connectedClient2GHzDut);
	    status = CommonMethods.isIpv4Address(ipv4AddressRetrievedFrom2GHzClient) && BroadBandConnectedClientUtils
		    .verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device, connectedClient2GHzDut);
	    errorMessage = "Client connected to 2.4 GHz private Wi-Fi haven't received valid IP Address from Gateway";
	    if (status) {
		LOGGER.info(
			"S3 ACTUAL: Connected Wireless client is assigned with the IPv4 Address between DHCP Range");
	    } else {
		LOGGER.error("S3 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 4: Verify the IPv6 Address is retrieved from the client connected to 2.4GHz Private Wi-Fi Network
	     *
	     */
	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 4: Verify the IPv6 Address is retrieved  from the client connected to 2.4GHz Private Wi-Fi Network");
	    LOGGER.info(
		    "STEP 4: ACTION : Get the device IPv6 address using below commandLinux : ifconfig wlan0 |grep -i \"inet addr:\"Windows: ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"IPv6 Address\"");
	    LOGGER.info("STEP 4: EXPECTED: Local IPv6 Address assigned to the client should be retrieved successfully");
	    String ipv6AddressRetrievedFrom2GHzClient = BroadBandConnectedClientUtils
		    .retrieveIPv6AddressFromConnectedClientWithDeviceCOnnected(connectedClient2GHzDut, tapEnv);
	    status = CommonMethods.isIpv6Address(ipv6AddressRetrievedFrom2GHzClient);
	    errorMessage = "Client connected to 2.4 GHz private Wi-Fi haven't received valid IP Address from Gateway";
	    if (status) {
		LOGGER.info("S4 ACTUAL: Local IPv6 Address assigned to the client is retrieved successfully");
	    } else {
		LOGGER.error("S4 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 5: Verify the internet is accessible in the client connected to 2.4 GHz Private Wi-Fi Network
	     * 
	     */
	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify the internet is accessible in the client connected to 2.4 GHz Private Wi-Fi Network");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the command in the client connected to 2.4 GHz private Wi-Fi : curl --connect-timeout 20 --head https://www.wikipedia.org");
	    LOGGER.info("STEP 5: EXPECTED : Internet should be accessible in the connected client.");
	    result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
		    connectedClient2GHzDut, BroadBandTestConstants.URL_WIKIPEDIA,
		    BroadBandTestConstants.EMPTY_STRING);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info("S5 ACTUAL: Internet is accessible from the client connected to 2.4 GHz private Wi-Fi");

	    } else {
		LOGGER.error("S5 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    LOGGER.info("#####################################################################################");

	    /**
	     * Step 6: Verify the client 2 connected to ethernet has IPv4 Address assigned from DHCP
	     * 
	     */
	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify the client 2 connected to ethernet has IPv4 Address assigned from DHCP");
	    LOGGER.info(
		    "STEP 6: ACTION : Get the device IPv4 address using below commandLinux : ifconfig eth0 |grep -i \"inet addr:\"Windows: ipconfig |grep -A 10 \"Ethernet LAN adapter Wi-Fi\" |grep -i \"IPv4 Address\"");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Client connected to the Ethernet should be assigned with the IP Address between DHCP Range");
	    connectedClientEthernetDut = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
	    status = null != connectedClientEthernetDut;
	    errorMessage = "Unable to retrieve the client connected to Ethernet";
	    String ipv4AddressRetrievedFromEthernetClient = null;
	    if (status) {
		ipv4AddressRetrievedFromEthernetClient = BroadBandConnectedClientUtils
			.getIpv4AddressFromConnClient(tapEnv, device, connectedClientEthernetDut);
		status = CommonMethods.isIpv4Address(ipv4AddressRetrievedFromEthernetClient)
			&& BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv,
				device, connectedClientEthernetDut);
		errorMessage = "Client connected to the Ethernet haven't received valid IPv4 Address from Gateway";
	    }
	    if (status) {
		LOGGER.info(
			"S6 ACTUAL: Client connected to the Ethernet is assigned with the IPv4 Address between DHCP Range");
	    } else {
		LOGGER.error("S6 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 7: Verify the IPv6 Address is retrieved from the client connected to Ethernet
	     *
	     */
	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Verify the IPv6 Address is retrieved  from the client connected to Ethernet");
	    LOGGER.info(
		    "STEP 7: ACTION : Get the device IPv6 address using below commandLinux : ifconfig eth0 |grep -i \"inet addr6:\"Windows: ipconfig |grep -A 10 \"Ethernet LAN adapter Wi-Fi\" |grep -i \"IPv6 Address\"");
	    LOGGER.info(
		    "STEP 7: EXPECTED : Local IPv6 Address assigned to the client should be retrieved successfully");
	    String ipv6AddressRetrievedFromEthernetClient = BroadBandConnectedClientUtils
		    .retrieveIPv6AddressFromConnectedClientWithDeviceCOnnected(connectedClientEthernetDut, tapEnv);
	    status = CommonMethods.isIpv6Address(ipv6AddressRetrievedFromEthernetClient);
	    errorMessage = "Client connected to ethernet haven't received valid IPv6 Address from Gateway";
	    if (status) {
		LOGGER.info("S7 ACTUAL: Local IPv6 Address assigned to the client is retrieved successfully");
	    } else {
		LOGGER.error("S7 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 8: Verify the internet is accessible in the client connected to Ethernet
	     * 
	     */
	    testStepNumber = "s8";
	    status = false;
	    LOGGER.info("STEP 8: DESCRIPTION : Verify the internet is accessible in the client connected to Ethernet");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute the command in connected client: curl --connect-timeout 20 --head https://www.wikipedia.org");
	    LOGGER.info("STEP 8: EXPECTED : Internet should be accessible in the connected client");
	    result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
		    connectedClientEthernetDut, BroadBandTestConstants.URL_WIKIPEDIA,
		    BroadBandTestConstants.EMPTY_STRING);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info("S8 ACTUAL: Internet is accessible from the client connected to ethernet");
	    } else {
		LOGGER.error("S8 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    LOGGER.info("#####################################################################################");

	    /**
	     * Step 9: Verify the Ping Connection for IPv4 Address is successful from 2.4GHz Wi-Fi client to Ethernet
	     * Client
	     *
	     */
	    testStepNumber = "s9";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify the Ping Connection for IPv4 Address is successful from 2.4GHz Wi-Fi client to Ethernet Client");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the command from the client connected to 2.4GHz : For Windows: ping –n 300 <IPv4 Address of Ethernet client>For Linux : ping –c 300 <IPv4 Address of 5Ethernet client>");
	    LOGGER.info(
		    "STEP 9: EXPECTED : Ping Connection should be successful from 2.4GHz Wi-Fi client to Ethernet client");
	    status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClient2GHzDut, tapEnv,
		    ipv4AddressRetrievedFromEthernetClient, BroadBandTestConstants.THIRTY_SECONDS);
	    errorMessage = "Ping Connection for IPv4 Address is not successful from 2.4GHz Wi-Fi client to Ethernet client";
	    if (status) {
		LOGGER.info(
			"S9 ACTUAL: Ping Connection for IPv4 Address is successful from 2.4GHz Wi-Fi client to Ethernet client");
	    } else {
		LOGGER.error("S9 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 10: Verify the Ping Connection for IPv6 Address is successful from 2.4GHz Wi-Fi client to Ethernet
	     * Client
	     *
	     */
	    testStepNumber = "s10";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Verify the Ping Connection for IPv6 Address is successful from 2.4GHz Wi-Fi client to Ethernet Client");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute the command from the client connected to 2.4GHz : For Windows: ping –n 300 <IPv6 Address of Ethernet client>For Linux : ping –c 300 <IPv6 Address of Ethernet client>");
	    LOGGER.info(
		    "STEP 10: EXPECTED : Ping Connection should be successful from 2.4GHz Wi-Fi client to Ethernet client");
	    status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClient2GHzDut, tapEnv,
		    ipv6AddressRetrievedFromEthernetClient, BroadBandTestConstants.THIRTY_SECONDS);
	    errorMessage = "Ping Connection for IPv6 Address is not successful from 2.4GHz Wi-Fi client to Ethernet client";
	    if (status) {
		LOGGER.info(
			"S10 ACTUAL: Ping Connection for IPv6 Address is successful from 2.4GHz Wi-Fi client to Ethernet client");
	    } else {
		LOGGER.error("S10 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 11: Verify the Ping Connection for IPv4 Address is successful from Ethernet client to 2.4GHz Wi-Fi
	     * client
	     *
	     */
	    testStepNumber = "s11";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Verify the Ping Connection  for IPv4 Address is successful from Ethernet client to 2.4GHz Wi-Fi client");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the command from the client connected to Ethernet : For Windows: ping –n 300 <IPv4 Address of 2.4 GHz client>For Linux : ping –c 300 <IPv4 Address of 2.4GHz client>");
	    LOGGER.info(
		    "STEP 11: EXPECTED : Ping Connection should be successful from Etherent client to 2.4GHz Wi-Fi client");
	    status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClientEthernetDut, tapEnv,
		    ipv4AddressRetrievedFrom2GHzClient, BroadBandTestConstants.THIRTY_SECONDS);
	    errorMessage = "Ping Connection for IPv4 Address is not successful from Ethernet client to 2.4GHz Wi-Fi client";
	    if (status) {
		LOGGER.info(
			"S11 ACTUAL: Ping Connection for IPv4 Address is successful from Ethernet client to 2.4GHz Wi-Fi client");
	    } else {
		LOGGER.error("S11 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 12: Verify the Ping Connection for IPv6 Address is successful from Ethernet client to 2.4GHz Wi-Fi
	     * client
	     *
	     */
	    testStepNumber = "s12";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify the Ping Connection  for IPv6 Address is successful from Ethernet client to 2.4GHz Wi-Fi client");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute the command from the client connected to Ethernet : For Windows: ping –n 300 <IPv6 Address of 2.4 GHz client>For Linux : ping –c 300 <IPv6 Address of 2.4GHz client>");
	    LOGGER.info(
		    "STEP 12: EXPECTED : Ping Connection should be successful from Ethernet client to 2.4GHz Wi-Fi client");
	    status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClientEthernetDut, tapEnv,
		    ipv6AddressRetrievedFrom2GHzClient, BroadBandTestConstants.THIRTY_SECONDS);
	    errorMessage = "Ping Connection for IPv6 Address is not successful from Ethernet client to 2.4GHz Wi-Fi client";
	    if (status) {
		LOGGER.info(
			"S12 ACTUAL: Ping Connection for IPv6 Address is successful from Ethernet client to 2.4GHz Wi-Fi client");
	    } else {
		LOGGER.error("S12 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception testException) {
	    errorMessage = testException.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE VERIFYING PING OPERATION BETWEEN CONNECTED CLIENTS: " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-INTRANET-ACC-1001");
    }

    /**
     * Verify the Ping operation is successful between 5 GHz Private Wi-Fi and Ethernet client
     * <ol>
     * <li>Verify the 5 GHz Private Wi-Fi SSID is enabled using WebPA</li>
     * <li>Connect the client 1 to 5 GHz Private Wi-Fi Network and verify connection status</li>
     * <li>Verify the IPv4 Address is retrieved from the client connected to 5 GHz Private Wi-Fi Network</li>
     * <li>Verify the IPv6 Address is retrieved from the client connected to 5 GHz Private Wi-Fi Network</li>
     * <li>Verify the internet is accessible in the client connected to 5 GHz Private Wi-Fi Network</li>
     * <li>Verify the client 2 connected to ethernet has IPv4 Address assigned from DHCP</li>
     * <li>Verify the IPv6 Address is retrieved from the client connected to Ethernet</li>
     * <li>Verify the internet is accessible in the client connected to Ethernet</li>
     * <li>Verify the Ping Connection for IPv4 Address is successful from 5 GHz Wi-Fi client to Ethernet Client</li>
     * <li>Verify the Ping Connection for IPv6 Address is successful from 5 GHz Wi-Fi client to Ethernet Client</li>
     * <li>Verify the Ping Connection for IPv4 Address is successful from Ethernet client to 5 GHz Wi-Fi client</li>
     * <li>Verify the Ping Connection for IPv6 Address is successful from Ethernet client to 5 GHz Wi-Fi client</li>
     * </ol>
     * @author SATHYA KISHORE
     * @refactor Athira
     * 
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-INTRANET-ACC-1002")
    public void testToVerifyIntranetConnectivityBetween5GhzAndEthernetClientUsingPing(Dut device) {

	String testId = "TC-RDKB-INTRANET-ACC-102";
	String testStepNumber = "s1";
	String errorMessage = null; // stores error message
	boolean status = false; // stores test steps status
	Dut connectedClient5GHzDut = null; // connected device to be verified
	Dut connectedClientEthernetDut = null; // connected device to be verified
	BroadBandResultObject result = null; // stores test result and error message

	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-INTRANET-ACC-1002");
	    LOGGER.info("TEST DESCRIPTION: Verify the Ping operation is successful between 5 GHz and Ethernet clients");

	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1. Verify the 5 GHz Private Wi-Fi SSID is enabled using WebPA");
	    LOGGER.info("2. Connect the client 1 to 5 GHz Private Wi-Fi Network and verify connection status ");
	    LOGGER.info(
		    "3. Verify the IPv4 Address is retrieved  from the client connected to 5 GHz Private Wi-Fi Network");
	    LOGGER.info(
		    "4. Verify the IPv6 Address is retrieved  from the client connected to 5 GHz Private Wi-Fi Network");
	    LOGGER.info("5. Verify the internet is accessible in the client connected to 5 GHz Private Wi-Fi Network");
	    LOGGER.info("6. Verify the client 2 connected to ethernet has IPv4 Address assigned from DHCP");
	    LOGGER.info("7. Verify the IPv6 Address is retrieved  from the client connected to Ethernet");
	    LOGGER.info("8. Verify the internet is accessible in the client connected to Ethernet");
	    LOGGER.info(
		    "9. Verify the Ping Connection for IPv4 Address is successful from 5 GHz Wi-Fi client to Ethernet Client");
	    LOGGER.info(
		    "10. Verify the Ping Connection for IPv6 Address is successful from 5 GHz Wi-Fi client to Ethernet Client");
	    LOGGER.info(
		    "11. Verify the Ping Connection  for IPv4 Address is successful from Ethernet client to 5 GHz Wi-Fi client");
	    LOGGER.info(
		    "12. Verify the Ping Connection  for IPv6 Address is successful from Ethernet client to 5 GHz Wi-Fi client");

	    LOGGER.info("#######################################################################################");

	    /**
	     * Step 1: Verify the 5 GHz Private Wi-Fi SSID is enabled using WebPA
	     *
	     */
	    testStepNumber = "s1";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 1: Verify the 5 GHz Private Wi-Fi SSID is enabled using WebPA");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the command to enable 5 GHz Private Wi-Fi SSID using parameter Device.WiFi.SSID.10101.Enable");
	    LOGGER.info("STEP 1: EXPECTED: 5 GHz private Wi-Fi radio should be enabled");
	    status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
		    WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, true);
	    errorMessage = "Enabling 5 GHz Private Wi-Fi SSID via WebPA failed";
	    if (status) {
		LOGGER.info("S1 ACTUAL: 5 GHz Private Wi-Fi SSID is enabled successfully using WebPA");
	    } else {
		LOGGER.error("S1 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 2: Connect the client 1 to 5 GHz Private Wi-Fi Network and verify connection status
	     *
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 2: Connect the client 1 to 5 GHz Private Wi-Fi Network and verify connection status");
	    LOGGER.info(
		    "STEP 2: ACTION : Connect to 5 GHz wifi using below commands Linux : nmcli dev wifi connect <ssid> password <passwd> Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
	    LOGGER.info("STEP 2: EXPECTED: Device should be connected with 5 GHz wifi network");
	    errorMessage = "Unable to connect to 5 GHz private Wi-Fi Network Or 5 GHz WiFi capable devices are not available";
	    try {
		connectedClient5GHzDut = BroadBandConnectedClientUtils
			.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = null != connectedClient5GHzDut;
	    if (status) {
		LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		LOGGER.info("S2 ACTUAL: Device has been connected with 5 GHz private Wi-Fi network");
	    } else {
		LOGGER.error("S2 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 3: Verify the IPv4 Address is retrieved from the client connected to 5 GHz Private Wi-Fi Network
	     *
	     */
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 3: Verify the IPv4 Address is retrieved  from the client connected to 5 GHz Private Wi-Fi Network");
	    LOGGER.info(
		    "STEP 3: ACTION : Get the device IPv4 address using below commandLinux : ifconfig wlan0 |grep -i \"inet addr:\"Windows: ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"IPv4 Address\"");
	    LOGGER.info(
		    "STEP 3: EXPECTED: Connected Wireless client should be assigned with the IP Address between DHCP Range");
	    String ipv4AddressRetrievedFrom5GHzClient = BroadBandConnectedClientUtils
		    .getIpv4AddressFromConnClient(tapEnv, device, connectedClient5GHzDut);
	    status = CommonMethods.isIpv4Address(ipv4AddressRetrievedFrom5GHzClient) && BroadBandConnectedClientUtils
		    .verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device, connectedClient5GHzDut);
	    errorMessage = "Client connected to 5 GHz private Wi-Fi haven't received valid IP Address from Gateway";
	    if (status) {
		LOGGER.info(
			"S3 ACTUAL: Connected Wireless client is assigned with the IPv4 Address between DHCP Range");
	    } else {
		LOGGER.error("S3 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 4: Verify the IPv6 Address is retrieved from the client connected to 5 GHz Private Wi-Fi Network
	     *
	     */
	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 4: Verify the IPv6 Address is retrieved  from the client connected to 5 GHz Private Wi-Fi Network");
	    LOGGER.info(
		    "STEP 4: ACTION : Get the device IPv6 address using below commandLinux : ifconfig wlan0 |grep -i \"inet addr:\"Windows: ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"IPv6 Address\"");
	    LOGGER.info("STEP 4: EXPECTED: Local IPv6 Address assigned to the client should be retrieved successfully");
	    String ipv6AddressRetrievedFrom5GHzClient = BroadBandConnectedClientUtils
		    .retrieveIPv6AddressFromConnectedClientWithDeviceCOnnected(connectedClient5GHzDut, tapEnv);
	    status = CommonMethods.isIpv6Address(ipv6AddressRetrievedFrom5GHzClient);
	    errorMessage = "Client connected to 5 GHz private Wi-Fi haven't received valid IP Address from Gateway";
	    if (status) {
		LOGGER.info("S4 ACTUAL: Local IPv6 Address assigned to the client is retrieved successfully");
	    } else {
		LOGGER.error("S4 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 5: Verify the internet is accessible in the client connected to 5 GHz Private Wi-Fi Network
	     * 
	     */
	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify the internet is accessible in the client connected to 5 GHz Private Wi-Fi Network");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the command in the client connected to 5 GHz private Wi-Fi : curl --connect-timeout 20 --head https://www.wikipedia.org");
	    LOGGER.info("STEP 5: EXPECTED : Internet should be accessible in the connected client.");
	    result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
		    connectedClient5GHzDut, BroadBandTestConstants.URL_WIKIPEDIA,
		    BroadBandTestConstants.EMPTY_STRING);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info("S5 ACTUAL: Internet is accessible from the client connected to 5 GHz private Wi-Fi");

	    } else {
		LOGGER.error("S5 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    LOGGER.info("#####################################################################################");

	    /**
	     * Step 6: Verify the client 2 connected to ethernet has IPv4 Address assigned from DHCP
	     * 
	     */
	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify the client 2 connected to ethernet has IPv4 Address assigned from DHCP");
	    LOGGER.info(
		    "STEP 6: ACTION : Get the device IPv4 address using below commandLinux : ifconfig eth0 |grep -i \"inet addr:\"Windows: ipconfig |grep -A 10 \"Ethernet LAN adapter Wi-Fi\" |grep -i \"IPv4 Address\"");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Client connected to the Ethernet should be assigned with the IP Address between DHCP Range");
	    connectedClientEthernetDut = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
	    status = null != connectedClientEthernetDut;
	    errorMessage = "Unable to retrieve the client connected to Ethernet";
	    String ipv4AddressRetrievedFromEthernetClient = null;
	    if (status) {
		ipv4AddressRetrievedFromEthernetClient = BroadBandConnectedClientUtils
			.getIpv4AddressFromConnClient(tapEnv, device, connectedClientEthernetDut);
		status = CommonMethods.isIpv4Address(ipv4AddressRetrievedFromEthernetClient)
			&& BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv,
				device, connectedClientEthernetDut);
		errorMessage = "Client connected to the Ethernet haven't received valid IPv4 Address from Gateway";
	    }
	    if (status) {
		LOGGER.info(
			"S6 ACTUAL: Client connected to the Ethernet is assigned with the IPv4 Address between DHCP Range");
	    } else {
		LOGGER.error("S6 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 7: Verify the IPv6 Address is retrieved from the client connected to Ethernet
	     *
	     */
	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Verify the IPv6 Address is retrieved  from the client connected to Ethernet");
	    LOGGER.info(
		    "STEP 7: ACTION : Get the device IPv6 address using below commandLinux : ifconfig eth0 |grep -i \"inet addr6:\"Windows: ipconfig |grep -A 10 \"Ethernet LAN adapter Wi-Fi\" |grep -i \"IPv6 Address\"");
	    LOGGER.info(
		    "STEP 7: EXPECTED : Local IPv6 Address assigned to the client should be retrieved successfully");
	    String ipv6AddressRetrievedFromEthernetClient = BroadBandConnectedClientUtils
		    .retrieveIPv6AddressFromConnectedClientWithDeviceCOnnected(connectedClientEthernetDut, tapEnv);
	    status = CommonMethods.isIpv6Address(ipv6AddressRetrievedFromEthernetClient);
	    errorMessage = "Client connected to ethernet haven't received valid IPv6 Address from Gateway";
	    if (status) {
		LOGGER.info("S7 ACTUAL: Local IPv6 Address assigned to the client is retrieved successfully");
	    } else {
		LOGGER.error("S7 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 8: Verify the internet is accessible in the client connected to Ethernet
	     * 
	     */
	    testStepNumber = "s8";
	    status = false;
	    LOGGER.info("STEP 8: DESCRIPTION : Verify the internet is accessible in the client connected to Ethernet");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute the command in connected client: curl --connect-timeout 20 --head https://www.wikipedia.org");
	    LOGGER.info("STEP 8: EXPECTED : Internet should be accessible in the connected client");
	    result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
		    connectedClientEthernetDut, BroadBandTestConstants.URL_WIKIPEDIA,
		    BroadBandTestConstants.EMPTY_STRING);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info("S8 ACTUAL: Internet is accessible from the client connected to ethernet");
	    } else {
		LOGGER.error("S8 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    LOGGER.info("#####################################################################################");

	    /**
	     * Step 9: Verify the Ping Connection for IPv4 Address is successful from 5 GHz Wi-Fi client to Ethernet
	     * Client
	     *
	     */
	    testStepNumber = "s9";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify the Ping Connection for IPv4 Address is successful from 5 GHz Wi-Fi client to Ethernet Client");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the command from the client connected to 5 GHz : For Windows: ping –n 300 <IPv4 Address of Ethernet client>For Linux : ping –c 300 <IPv4 Address of 5Ethernet client>");
	    LOGGER.info(
		    "STEP 9: EXPECTED : Ping Connection should be successful from 5 GHz Wi-Fi client to Ethernet client");
	    status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClient5GHzDut, tapEnv,
		    ipv4AddressRetrievedFromEthernetClient, BroadBandTestConstants.THIRTY_SECONDS);
	    errorMessage = "Ping Connection for IPv4 Address is not successful from 5 GHz Wi-Fi client to Ethernet client";
	    if (status) {
		LOGGER.info(
			"S9 ACTUAL: Ping Connection for IPv4 Address is successful from 5 GHz Wi-Fi client to Ethernet client");
	    } else {
		LOGGER.error("S9 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 10: Verify the Ping Connection for IPv6 Address is successful from 5 GHz Wi-Fi client to Ethernet
	     * Client
	     *
	     */
	    testStepNumber = "s10";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Verify the Ping Connection for IPv6 Address is successful from 5 GHz Wi-Fi client to Ethernet Client");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute the command from the client connected to 5 GHz : For Windows: ping –n 300 <IPv6 Address of Ethernet client>For Linux : ping –c 300 <IPv6 Address of Ethernet client>");
	    LOGGER.info(
		    "STEP 10: EXPECTED : Ping Connection should be successful from 5 GHz Wi-Fi client to Ethernet client");
	    status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClient5GHzDut, tapEnv,
		    ipv6AddressRetrievedFromEthernetClient, BroadBandTestConstants.THIRTY_SECONDS);
	    errorMessage = "Ping Connection for IPv6 Address is not successful from 5 GHz Wi-Fi client to Ethernet client";
	    if (status) {
		LOGGER.info(
			"S10 ACTUAL: Ping Connection for IPv6 Address is successful from 5 GHz Wi-Fi client to Ethernet client");
	    } else {
		LOGGER.error("S10 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 11: Verify the Ping Connection for IPv4 Address is successful from Ethernet client to 5 GHz Wi-Fi
	     * client
	     *
	     */
	    testStepNumber = "s11";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Verify the Ping Connection  for IPv4 Address is successful from Ethernet client to 5 GHz Wi-Fi client");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the command from the client connected to Ethernet : For Windows: ping –n 300 <IPv4 Address of 5 GHz client>For Linux : ping –c 300 <IPv4 Address of 2.4GHz client>");
	    LOGGER.info(
		    "STEP 11: EXPECTED : Ping Connection should be successful from Etherent client to 5 GHz Wi-Fi client");
	    status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClientEthernetDut, tapEnv,
		    ipv4AddressRetrievedFrom5GHzClient, BroadBandTestConstants.THIRTY_SECONDS);
	    errorMessage = "Ping Connection for IPv4 Address is not successful from Ethernet client to 5 GHz Wi-Fi client";
	    if (status) {
		LOGGER.info(
			"S11 ACTUAL: Ping Connection for IPv4 Address is successful from Ethernet client to 5 GHz Wi-Fi client");
	    } else {
		LOGGER.error("S11 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 12: Verify the Ping Connection for IPv6 Address is successful from Ethernet client to 5 GHz Wi-Fi
	     * client
	     *
	     */
	    testStepNumber = "s12";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify the Ping Connection  for IPv6 Address is successful from Ethernet client to 5 GHz Wi-Fi client");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute the command from the client connected to Ethernet : For Windows: ping –n 300 <IPv6 Address of 5 GHz client>For Linux : ping –c 300 <IPv6 Address of 2.4GHz client>");
	    LOGGER.info(
		    "STEP 12: EXPECTED : Ping Connection should be successful from Ethernet client to 5 GHz Wi-Fi client");
	    status = ConnectedNattedClientsUtils.verifyPingConnection(connectedClientEthernetDut, tapEnv,
		    ipv6AddressRetrievedFrom5GHzClient, BroadBandTestConstants.THIRTY_SECONDS);
	    errorMessage = "Ping Connection for IPv6 Address is not successful from Ethernet client to 5 GHz Wi-Fi client";
	    if (status) {
		LOGGER.info(
			"S12 ACTUAL: Ping Connection for IPv6 Address is successful from Ethernet client to 5 GHz Wi-Fi client");
	    } else {
		LOGGER.error("S12 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception testException) {
	    errorMessage = testException.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE VERIFYING PING OPERATION BETWEEN CONNECTED CLIENTS: " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-INTRANET-ACC-1002");
    }

}
