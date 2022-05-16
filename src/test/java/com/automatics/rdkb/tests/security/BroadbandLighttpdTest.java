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

package com.automatics.rdkb.tests.security;

import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.selfheal.BroadBandSelfHealUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaServerResponse;

public class BroadbandLighttpdTest extends AutomaticsTestBase {

    /** command to get log upload details */
    public static final String CMD_GET_LOG_UPLOAD_DETAILS = "tail -F /rdklogs/logs/dcmscript.log > /nvram/automation_test.txt";

    /**
     * Test to verify LAN - WAN traffic is successful when Firewall is configured to Custom Security
     * 
     * <ol>
     * <li>STEP 1: Verify whether the firewall setting is configured to 'Custom Security' for IPv4 traffic</li>
     * <li>STEP 2: Verify whether the Custom Secuirty is configured to 'Block HTTP' for IPv4 traffic</li>
     * <li>STEP 3: Verify whether the Custom Secuirty is configured to 'Block HTTPs' for IPv4 traffic</li>
     * <li>STEP 4: Verify whether Private Wi-Fi SSID is enabled using WebPA</li>
     * <li>STEP 5: Connect the device to private Wi-Fi SSID and verify connection status</li>
     * <li>STEP 6: Check if the wirless connected client has an IP address from the gateway</li>
     * <li>STEP 7: Verify the traffic from WLAN to WAN should be successful for Ipv4 traffic even when HTTP/HTTPs
     * requests are blocked under Firewall settings</li>
     * <li>STEP 8: Verify the traffic from LAN to WAN should be successful for Ipv4 traffic even when HTTP/HTTPs
     * requests are blocked under Firewall settings</li>
     * <li>STEP 9: Verify whether the firewall setting is configured to 'Custom Security' for IPv6 traffic</li>
     * <li>STEP 10: Verify whether the Custom Secuirty is configured to 'Block HTTP' for IPv6 traffic</li>
     * <li>STEP 11: Verify whether the Custom Secuirty is configured to 'Block HTTPs' for IPv6 traffic</li>
     * <li>STEP 12: Verify the traffic from WLAN to WAN should be successful for IPv6 trafficeven when HTTP/HTTPs
     * requests are blocked under Firewall settings</li>
     * <li>STEP 13: Verify the traffic from LAN to WAN should be successful for IPv6 trafficeven when HTTP/HTTPs
     * requests are blocked under Firewall settings</li>
     * <li>POST-CONDITION 1: Verify whether the 'Block HTTP' for IPv4 traffic can be disabled</li>
     * <li>POST-CONDITION 2: Verify whether the 'Block HTTPs' for IPv4 traffic can be disabled</li>
     * <li>POST-CONDITION 3: Verify whether the firewall setting is configured to 'Minimum Security' for IPv4
     * traffic</li>
     * <li>POST-CONDITION 4: Verify whether the 'Block HTTP' for IPv6 traffic can be disabled</li>
     * <li>POST-CONDITION 5: Verify whether the 'Block HTTPs' for IPv6 traffic can be disabled</li>
     * <li>POST-CONDITION 6: Verify whether the firewall setting is configured to 'Typical Security' for IPv6
     * traffic</li>
     * </ol>
     * 
     * @param device
     * 
     * @refactor yamini.s
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-FIREWALL-1001")
    public void testToVerifyLantoWantrafficInCustomFirewal(Dut device) {

	String testId = "TC-RDKB-FIREWALL-101";
	int stepNumber = 1;
	String testStepNumber = "S" + stepNumber;
	String errorMessage = null;
	boolean status = false;
	Dut connectedClientSettop = null; // connected device to be verified
	BroadBandResultObject result = null; // stores test result and error message

	try {

	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-FIREWALL-1001 #####################");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify LAN - WAN traffic is successful when Firewall is configured to 'Custom Security'");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1: Verify the firewall setting is configured to 'Custom Security' for IPv4 traffic");
	    LOGGER.info("2: Verify the Custom Secuirty is configured to 'Block HTTP' for IPv4 traffic");
	    LOGGER.info("3: Verify the Custom Secuirty is configured to 'Block HTTPs' for IPv4 traffic");
	    LOGGER.info("4: Verify whether Private Wi-Fi SSID  is enabled using WebPA");
	    LOGGER.info("5: Connect the device to private Wi-Fi SSID and verify connection status");
	    LOGGER.info("6: Check if the wirless connected client has an IP address from the gateway");
	    LOGGER.info(
		    "7: Verify the traffic from WLAN to WAN should be successful for Ipv4 traffic even when HTTP/HTTPs requests are blocked under Firewall settings");
	    LOGGER.info("8: Verify the firewall setting is configured to 'Custom Security' for IPv6 traffic");
	    LOGGER.info("9: Verify the Custom Secuirty is configured to 'Block HTTP' for IPv6 traffic");
	    LOGGER.info("10: Verify the Custom Secuirty is configured to 'Block HTTPs' for IPv6 traffic");
	    LOGGER.info(
		    "11: Verify the traffic from WLAN to WAN should be successful for IPv6 trafficeven when HTTP/HTTPs requests are blocked under Firewall settings");
	    LOGGER.info("POST-CONDITION 1: Verify whether the 'Block HTTP' for IPv4 traffic can be disabled");
	    LOGGER.info("POST-CONDITION 2: Verify whether the 'Block HTTPs' for IPv4 traffic can be disabled");
	    LOGGER.info(
		    "POST-CONDITION 3: Verify whether the firewall setting is configured to 'Minimum Security' for IPv4 traffic");
	    LOGGER.info("POST-CONDITION 4: Verify whether the 'Block HTTP' for IPv6 traffic can be disabled");
	    LOGGER.info("POST-CONDITION 5: Verify whether the 'Block HTTPs' for IPv6 traffic can be disabled");
	    LOGGER.info(
		    "POST-CONDITION 6: Verify whether the firewall setting is configured to 'Typical Security' for IPv6 traffic");
	    LOGGER.info("#####################################################################################");

	    /**
	     * Step 1: Verify the firewall setting is configured to 'Custom Security' for IPv4 traffic
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the firewall setting is configured to 'Custom Security' for IPv4 traffic");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute WebpA command Device.X_CISCO_COM_Security.Firewall.FirewallLevel to to Custom Security");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: Firewall Setting for IPv4 traffic should be set to Custom Security");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL, WebPaDataTypes.STRING.getValue(),
		    BroadBandTestConstants.FIREWALL_CUSTOM_SECURITY);
	    errorMessage = "Firewall Setting for IPv4 traffic cannot be set to Custom Security";
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Firewall Setting for IPv4 traffic is successfully set to Custom Security");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 2: Verify the Custom Secuirty is configured to 'Block HTTP' for IPv4 traffic
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the Custom Secuirty is configured to 'Block HTTP' for IPv4 traffic");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute WebpA command Device.X_CISCO_COM_Security.Firewall.FilterHTTP to to true");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: Firewall Setting for IPv4 traffic should be set to 'Block HTTP' under Custom Security");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_HTTP_FOR_IPV4_TRAFFIC_UNDER_CUSTOM_FIREWALL,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    errorMessage = "Firewall Setting for IPv4 traffic cannot be set to 'Block HTTP' under Custom Security";
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Firewall Setting for IPv4 traffic is successfully set to 'Block HTTP' under Custom Security");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 3: Verify the Custom Security is configured to 'Block HTTPs' for IPv4 traffic
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the Custom Secuirty is configured to 'Block HTTPs' for IPv4 traffic");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute WebpA command Device.X_CISCO_COM_Security.Firewall.FilterHTTPs to to true");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: Firewall Setting for IPv4 traffic should be set to 'Block HTTPs' under Custom Security");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_HTTPS_FOR_IPV4_TRAFFIC_UNDER_CUSTOM_FIREWALL,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    errorMessage = "Firewall Setting for IPv4 traffic cannot be set to 'Block HTTPs' under Custom Security";
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Firewall Setting for IPv4 traffic is successfully set to 'Block HTTPs' under Custom Security");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 4: Verify whether Private Wi-Fi SSIDs are enabled using WebPA
	     *
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether Private Wi-Fi SSIDs are enabled using WebPA");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute WebpA command Device.WiFi.SSID.10001.Enable and Device.WiFi.SSID.10101.Enable to true");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED: Both 2.4 GHz & 5 GHz private Wi-Fi radios should be enabled");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, true)
		    && BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
			    WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, true);
	    errorMessage = "Enabling private Wi-Fi SSIDs' via WebPA failed";
	    if (status) {
		LOGGER.info(
			testStepNumber + " ACTUAL: Both 2.4 GHz & 5 GHz Private Wi-Fi SSIDs' are enabled using WebPA");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 5: Connect the device to the private Wi-Fi SSID and verify connection status
	     * 
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Connect the device to the private Wi-Fi SSID and verify connection status");
	    LOGGER.info("STEP " + stepNumber + ": ACTION: Connect to private Wi-Fi and verify connection status");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should be connected to private Wi-Fi network");
	    LOGGER.info("**********************************************************************************");
	    try {
		errorMessage = "Unable to connect to 2.4 GHz private Wi-Fi Network";
		connectedClientSettop = BroadBandConnectedClientUtils.getWindowsClientsAndConnectToGivenSSID(device,
			tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    } catch (TestException e) {
		try {
		    errorMessage = "Unable to connect to both 2.4 GHz & 5 GHz private Wi-Fi Network";
		    connectedClientSettop = BroadBandConnectedClientUtils.getWindowsClientsAndConnectToGivenSSID(device,
			    tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
		} catch (TestException ex) {
		    errorMessage = ex.getMessage();
		}
	    }
	    status = null != connectedClientSettop;
	    if (status) {

		LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		LOGGER.info(
			testStepNumber + " ACTUAL: Device has been connected succesfully with private Wi-Fi network");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 6: Check if the wireless connected client has an IP address from the gateway
	     *
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Check if the wireless connected client has an IP address from the gateway");
	    LOGGER.info("STEP " + stepNumber + ": ACTION: Get the device IPv4 address");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : DHCP Range IP Address should be assigned to the Wireless Connected device");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
		    connectedClientSettop);
	    errorMessage = "Cilent connected to the private Wi-Fi haven't received valid IP Address from Gateway";
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Client connected to the private Wi-Fi network has got IP Address from Gateway");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 7: Verify the traffic from WLAN to WAN should be successful for Ipv4 traffic even when HTTP/HTTPs
	     * requests are blocked under Firewall settings
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the traffic from WLAN to WAN should be successful for Ipv4 traffic even when HTTP/HTTPs requests are blocked under Firewall settings");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute command from the connected client: 'curl --connect-timeout 20 -v -4 https://www.google.com'");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Firewall settings should not restrict WLAN to WAN traffic, connection should be successful");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
		    connectedClientSettop, BroadBandTestConstants.URL_WIKIPEDIA, BroadBandTestConstants.IP_VERSION4);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Firewall settings didn't restrict WLAN to WAN traffic as expected, connection is successful");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 8: Verify the firewall setting is configured to 'Custom Security' for IPv6 traffic
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the firewall setting is configured to 'Custom Security' for IPv6 traffic");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute WebPA command Device.X_CISCO_COM_Security.Firewall.FirewallLevelV6 to custom");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: Firewall Setting for IPv6 traffic should be set to Custom Security");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL_IPV6, WebPaDataTypes.STRING.getValue(),
		    BroadBandTestConstants.FIREWALL_CUSTOM_SECURITY);
	    errorMessage = "Firewall Setting for IPv6 traffic cannot be set to Custom Security";
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Firewall Setting for IPv6 traffic is successfully set to Custom Security");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 9: Verify the Custom Secuirty is configured to 'Block HTTP' for IPv6 traffic
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the Custom Secuirty is configured to 'Block HTTP' for IPv6 traffic");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute WebPA command Device.X_CISCO_COM_Security.Firewall.FilterHTTPV6 to true");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: Firewall Setting for IPv6 traffic should be set to 'Block HTTP' under Custom Security");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_HTTP_FOR_IPV6_TRAFFIC_UNDER_CUSTOM_FIREWALL,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    errorMessage = "Firewall Setting for IPv6 traffic cannot be set to 'Block HTTP' under Custom Security";
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Firewall Setting for IPv6 traffic is successfully set to 'Block HTTP' under Custom Security");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 10: Verify the Custom Secuirty is configured to 'Block HTTPs' for IPv6 traffic
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the Custom Secuirty is configured to 'Block HTTPs' for IPv6 traffic");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute WebPA command Device.X_CISCO_COM_Security.Firewall.FilterHTTPsV6 to true");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: Firewall Setting for IPv6 traffic should be set to 'Block HTTPs' under Custom Security");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_HTTPS_FOR_IPV6_TRAFFIC_UNDER_CUSTOM_FIREWALL,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    errorMessage = "Firewall Setting for IPv6 traffic cannot be set to 'Block HTTPs' under Custom Security";
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Firewall Setting for IPv6 traffic is successfully set to 'Block HTTPs' under Custom Security");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 11: Verify the traffic from LAN to WAN should be successful for Ipv6 traffic even when HTTP/HTTPs
	     * requests are blocked under Firewall settings
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the traffic from WLAN to WAN should be successful for Ipv6 traffic even when HTTP/HTTPs requests are blocked under Firewall settings");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute command from the connected client: 'curl --connect-timeout 20 -v -6 https://www.google.com'");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Firewall settings should not restrict WLAN to WAN traffic, connection should be successful");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
		    connectedClientSettop, BroadBandTestConstants.URL_WIKIPEDIA, BroadBandTestConstants.IP_VERSION6);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Firewall settings didn't restrict WLAN to WAN IPv6 traffic as expected, connection is successful");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception testException) {
	    errorMessage = testException.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING LAN - WAN TRAFFIC IS SUCCESSFUL IN CUSTOM FIREWALL : "
		    + errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 1: DESCRIPTION: Verify whether the 'Block HTTP' for IPv4 traffic can be disabled");
	    LOGGER.info(
		    "POST-CONDITION 1: EXPECTED: 'Block HTTP' under Custom Security should be disabled for IPv4 Traffic");
	    LOGGER.info("#######################################################################################");
	    if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_HTTP_FOR_IPV4_TRAFFIC_UNDER_CUSTOM_FIREWALL,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE)) {
		LOGGER.info(
			"POST-CONDITION-1 PASSED: 'Block HTTP' under Custom Security is disabled successfully for IPv4 Traffic");
	    } else {
		LOGGER.error(
			"POST-CONDITION-1 FAILED: 'Block HTTP' under Custom Security cannot be disabled for IPv4 Traffic");
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 2: DESCRIPTION: Verify whether the 'Block HTTPs' for IPv4 traffic can be disabled");
	    LOGGER.info(
		    "POST-CONDITION 2: EXPECTED: 'Block HTTPs' under Custom Security should be disabled for IPv4 Traffic");
	    LOGGER.info("#######################################################################################");
	    if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_HTTPS_FOR_IPV4_TRAFFIC_UNDER_CUSTOM_FIREWALL,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE)) {
		LOGGER.info(
			"POST-CONDITION-2 PASSED: 'Block HTTPs' under Custom Security is disabled successfully for IPv4 Traffic");
	    } else {
		LOGGER.error(
			"POST-CONDITION-2 FAILED: 'Block HTTPs' under Custom Security cannot be disabled for IPv4 Traffic");
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 3: DESCRIPTION: Verify whether the firewall setting is configured to 'Minimum Security' for IPv4 traffic");
	    LOGGER.info(
		    "POST-CONDITION 3: EXPECTED: Firewall Setting for IPv4 traffic should be set to 'Minimum Security'");
	    LOGGER.info("#######################################################################################");
	    if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL, WebPaDataTypes.STRING.getValue(),
		    BroadBandTestConstants.FIREWALL_IPV4_MINIMUM_SECURITY)) {
		LOGGER.info(
			"POST-CONDITION-3 PASSED: Firewall Setting for IPv4 traffic is set successfully to 'Minimum Security'");
	    } else {
		LOGGER.error(
			"POST-CONDITION-3 FAILED: Firewall Setting for IPv4 traffic cannot be set to 'Minimum Security'");
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 4: DESCRIPTION: Verify whether the 'Block HTTP' for IPv6 traffic can be disabled");
	    LOGGER.info(
		    "POST-CONDITION 4: EXPECTED: 'Block HTTP' under Custom Security should be disabled for IPv6 Traffic");
	    LOGGER.info("#######################################################################################");
	    if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_HTTP_FOR_IPV6_TRAFFIC_UNDER_CUSTOM_FIREWALL,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE)) {
		LOGGER.info(
			"POST-CONDITION-4 PASSED: 'Block HTTP' under Custom Security is disabled successfully for IPv6 Traffic");
	    } else {
		LOGGER.error(
			"POST-CONDITION-4 FAILED: 'Block HTTP' under Custom Security cannot be disabled for IPv6 Traffic");
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 5: DESCRIPTION: Verify whether the 'Block HTTPs' for IPv6 traffic can be disabled");
	    LOGGER.info(
		    "POST-CONDITION 5: EXPECTED: 'Block HTTPs' under Custom Security should be disabled for IPv6 Traffic");
	    LOGGER.info("#######################################################################################");
	    if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_HTTPS_FOR_IPV6_TRAFFIC_UNDER_CUSTOM_FIREWALL,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE)) {
		LOGGER.info(
			"POST-CONDITION-5 PASSED: 'Block HTTPs' under Custom Security is disabled successfully for IPv6 Traffic");
	    } else {
		LOGGER.error(
			"POST-CONDITION-5 FAILED: 'Block HTTPs' under Custom Security cannot be disabled for IPv6 Traffic");
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "POST-CONDITION 6: DESCRIPTION: Verify whether the firewall setting is configured to 'Typical Security' for IPv6 traffic");
	    LOGGER.info(
		    "POST-CONDITION 6: EXPECTED: Firewall Setting for IPv6 traffic should be set to 'Typical Security'");
	    LOGGER.info("#######################################################################################");
	    if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL_IPV6, WebPaDataTypes.STRING.getValue(),
		    BroadBandTestConstants.FIREWALL_IPV6_TYPICAL_SECURITY)) {
		LOGGER.info(
			"POST-CONDITION-6 PASSED: Firewall Setting for IPv6 traffic is set successfully to 'Typical Security'");
	    } else {
		LOGGER.error(
			"POST-CONDITION-6 FAILED: Firewall Setting for IPv6 traffic cannot be set to 'Typical Security'");
	    }
	    LOGGER.info("########################### COMPLETED POST-CONFIGURATIONS ###########################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-FIREWALL-1001");
    }

    /**
     * Test to verify diagnostic mode test scenarios
     * 
     * 
     * <ol>
     * <p>
     * <li>STEP 1 :Enable diagnostic mode using webpa params</li>
     * <li>STEP 2 : Disconnect DUT from Internet and verify wifi status</li>
     * <li>STEP 3,4,5,6 : Verify ping failures</li>
     * <li>STEP 7 : Reconnect DUT from Internet and verify wifi status</li>
     * <li>STEP 8,9,10,11 :Verify ping status</li>
     * <li>STEP 12,13 :verify logupload frequency and verify</li>
     * <li>STEP 14,15 : Configure ping data block size and verify using system commands</li>
     * </p>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @author Gnanaprakasham S
     * @refactor yamini.s
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-SELF-HEAL-4004")

    public void testVerifySelfHealDiagnosticModeScenarios(Dut device) {

	// String to store the test case status
	boolean status = false;
	// Test case id
	String testId = "TC-RDKB-SELF-HEAL-404";
	// Test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// String variable to store command response
	String response = null;
	// connected device to be verified
	Dut connectedDeviceActivated = null;
	boolean t2Enabled = false;
	try {

	    LOGGER.info("STARTING TEST CASE: TC-RDKB-SELF-HEAL-4004");

	    LOGGER.info("**************************************************************");
	    LOGGER.info("TEST DESCRIPTION: Test to verify self heal diagnostic mode test scenarios using webpa params");
	    LOGGER.info("*************************************************************************");

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info(
		    "STEP 1: Enable the diagnostic mode using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" ");
	    LOGGER.info("EXPECTED: Diagnostic mode must be enabled via Tr181 dataobject ");
	    LOGGER.info(
		    "STEP 2: When the device is in diagnostic mode, unplug the DUT from the internet and verify wifi enabled status  ");
	    LOGGER.info("EXPECTED: Wifi network must be disabled from gateway device");
	    LOGGER.info("STEP 3:Verify whether interface got the correct IPv4  address.");
	    LOGGER.info("EXPECTED:Interface IP address should not be shown");
	    LOGGER.info("STEP 4:Verify whether interface got the correct IPv6  address.");
	    LOGGER.info("EXPECTED:Interface IP address should not be shown");
	    LOGGER.info("STEP 5: Verify whether you have connectivity using that particular interface using IPV4 ");
	    LOGGER.info("EXPECTED: Connectivity check should return connection failure error message");
	    LOGGER.info("STEP 6: Verify whether you have connectivity using that particular interface using IPV6 ");
	    LOGGER.info("EXPECTED: Connectivity check should return connection failure error message ");
	    LOGGER.info(
		    "STEP 7: When the device is in diagnostic mode, reconnect the DUT from the internet and verify wifi enabled status");
	    LOGGER.info("EXPECTED: Wifi network must be enabled from gateway device");
	    LOGGER.info("STEP 8:Verify whether interface got the correct IPv4  address.");
	    LOGGER.info("EXPECTED:Interface IP address should be shown");
	    LOGGER.info("STEP 9:Verify whether interface got the correct IPv6  address.");
	    LOGGER.info("EXPECTED:Interface IP address should be shown");
	    LOGGER.info("STEP 10: Verify whether you have connectivity using that particular interface using IPV4 ");
	    LOGGER.info("EXPECTED: Connectivity check should return status as 200");
	    LOGGER.info("STEP 11: Verify whether you have connectivity using that particular interface using IPV6 ");
	    LOGGER.info("EXPECTED: Connectivity check should return status as 200");
	    LOGGER.info(
		    "STEP 12: Change the log upload frequency using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency\" ");
	    LOGGER.info(
		    "EXPECTED: Logupload frequency value must be changed to other than default value (for eg: 2 mins )");
	    LOGGER.info(
		    "STEP 13: Verify whether the device send the stored messages for configured upload frequency duration  ");
	    LOGGER.info("EXPECTED: Device must upload logs for every 2 minutes ");
	    LOGGER.info(
		    "STEP 14: Configure the ping data block size using \"Device.IP.Diagnostics.IPPing.DataBlockSize\" TR-069/WebPA parameter and verify status");
	    LOGGER.info("EXPECTED: Ping data block size must be changed to 100 ");
	    LOGGER.info("STEP 15: Verify ping data block size using system commands  ");
	    LOGGER.info("EXPECTED: Ping data block size must be changed to 100");

	    LOGGER.info(
		    "************************************************************************************************");

	    // Enabled diagnostic mode using webpa params

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION: Enable the diagnostic mode using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" ");
	    LOGGER.info(
		    "STEP 1: ACTION: Execute webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" and verify diagnostic mode");
	    LOGGER.info("STEP 1:EXPECTED: Diagnostic mode must be enabled via Tr181 dataobject ");
	    LOGGER.info(
		    "************************************************************************************************");

	    try {
		errorMessage = "Not able to enable diagnostic mode using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagnosticMode\" ";
		status = BroadBandSelfHealUtils.enableOrDisableDiagnosticModeUsingWebpaParams(device, tapEnv,
			BroadBandTestConstants.TRUE);
		LOGGER.info("Diagnostic mode enabled status via webpa params: " + status);
	    } catch (Exception exception) {
		errorMessage = "Exception occurred during execution : " + exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("S1 ACTUAL RESULT : "
		    + (status ? "SUCCESSFULLY ENABLED DIAGNOSTIC MODE UISNG WEBPA PARAMETER !!!" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info(
		    "STEP 2:DESCRIPTION: When the device is in diagnostic mode, unplug the DUT from the internet and verify wifi enabled status  ");
	    LOGGER.info(
		    "STEP 2: ACTION: Set webpa params \"Device.WiFi.SSID.1.Enable\" and \"Device.WiFi.SSID.e.Enable\" to false ");
	    LOGGER.info("STEP 2: EXPECTED: Wifi network must be disabled from gateway device");
	    LOGGER.info(
		    "************************************************************************************************");

	    testStepNumber = "s2";
	    status = false;

	    try {

		// Get the client device which is connected with wifi network
		connectedDeviceActivated = BroadBandConnectedClientUtils
			.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
		errorMessage = "Not able to diable 2.4 GHz and 5 GHz radio using webpa param Device.WiFi.SSID.{i}.Enable";
		status = BroadBandConnectedClientUtils.enableOrDisableAllRadios(device, tapEnv, false);
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTES);
	    } catch (Exception exception) {
		errorMessage = "Exception occurred during execution : " + exception.getMessage();
		LOGGER.error(errorMessage);
	    }

	    LOGGER.info("S2 ACTUAL RESULT : "
		    + (status ? "SUCCESSFULLY DISABLED 2.4 GHZ AND 5 GHZ RADIOS UISNG WEBPA PARAMETER !!!"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Steps 3 to 6
	     */
	    /**
	     * Verify whether connected client devices having proper ip address and connectivity when wifi disabled
	     */

	    try {
		if (null != connectedDeviceActivated) {
		    BroadBandConnectedClientUtils.checkIpAddressAndConnectivityAfterWifiDisabled(device, tapEnv,
			    connectedDeviceActivated, testId, new String[] { "s3", "s4", "s5", "s6" });
		} else {
		    errorMessage = "Unable to connect to 2.4GHz private SSID when 5GHZ Private SSID disabled mode";
		}
	    } catch (Exception exception) {
		errorMessage = "Exception occurred during execution : " + exception.getMessage();
		LOGGER.error(errorMessage);
	    }

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION :When the device is in diagnostic mode, reconnect the DUT from the internet and verify wifi enabled status");
	    LOGGER.info(
		    "STEP 7: ACTION: Set webpa params \"Device.WiFi.SSID.1.Enable\" and \"Device.WiFi.SSID.e.Enable\" to true ");
	    LOGGER.info("STEP 7:EXPECTED: Wifi network must be enabled from gateway device");
	    LOGGER.info(
		    "************************************************************************************************");

	    testStepNumber = "s7";
	    status = false;

	    errorMessage = "Not able to enable 2.4 GHz and 5 GHz radio using webpa param Device.WiFi.SSID.{i}.Enable";
	    status = BroadBandConnectedClientUtils.enableOrDisableAllRadios(device, tapEnv, true);

	    LOGGER.info("S7 ACTUAL RESULT : "
		    + (status ? "SUCCESSFULLY ENABLED 2.4 GHZ AND 5 GHZ RADIOS UISNG WEBPA PARAMETER !!!"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Steps 8 to 11
	     */
	    /**
	     * Verify whether connected client devices having proper ip address and connectivity after reconnecting with
	     * wifi
	     */

	    // SSID of 2.4GHz Wi-Fi Network
	    String ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    // Pass Phrase of 2.4GHz Wi-Fi Network
	    String passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device,
		    tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName, passPhraseName);

	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);

	    try {
		BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, connectedDeviceActivated,
			testId, new String[] { "s8", "s9", "s10", "s11" });
	    } catch (Exception exception) {

	    }

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION: Change the log upload frequency using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency\" ");
	    LOGGER.info(
		    "STEP 12: ACTION: Set verify log upload frequency as 2 mins by execute webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency\"");
	    LOGGER.info(
		    "STEP 12: EXPECTED: Logupload frequency value must be changed to other than default value (for eg: 2 mins )");
	    LOGGER.info(
		    "************************************************************************************************");

	    testStepNumber = "s12";
	    status = false;

	    errorMessage = "Not able to configure log upload frequency using webpa param \"Device.SelfHeal.X_RDKCENTRAL-COM_DiagMode_LogUploadFrequency\"";
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DIAG_MODE_LOG_UPLOAD_FREQUENCY,
		    WebPaDataTypes.INTEGER.getValue(), BroadBandTestConstants.STRING_VALUE_TWO);

	    LOGGER.info("S12 ACTUAL RESULT : "
		    + (status ? "SUCCESSFULLY CHANGED LOG UPLOAD FREQUENCY UISNG WEBPA PARAMETER !!!" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s13";
	    status = false;

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info(
		    "STEP 13: Verify whether the device send the stored messages for configured upload frequency duration  ");
	    LOGGER.info(
		    "ACTION: Execute command \"cat /rdklogs/logs/dcmscript.log\" and verify log upload time difference ");
	    LOGGER.info("EXPECTED: Device must upload logs for every 2 minutes ");
	    LOGGER.info(
		    "************************************************************************************************");

	    t2Enabled = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE);
	    if (t2Enabled) {
		errorMessage = "THIS STEP IS NOT APPLICABLE FOR T2.0 ENABLE DEVICES";
		LOGGER.error("STEP " + testStepNumber + ": ACTUAL : " + errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    } else {

		tapEnv.executeCommandUsingSsh(device, CMD_GET_LOG_UPLOAD_DETAILS);
		LOGGER.info("Waiting for 15 minutes");
		long waitTime = (CommonMethods.isAtomSyncAvailable(device, tapEnv))
			? BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS
			: BroadBandTestConstants.FIVE_MINUTES;
		tapEnv.waitTill(waitTime);
		response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_GET_LOG_UPLOAD_DETAILS);
		errorMessage = "Device is not uploading log to server for every 2 minutes aven after changing the log upload frequency to 2 minutes";
		if (CommonMethods.isNotNull(response)) {
		    LOGGER.info("Obtained response for log upload : " + response);
		    status = CommonMethods.patternMatcher(response,
			    BroadBandTestConstants.PATTERN_GET_LOG_UPLOAD_TIMING);
		} else {
		    errorMessage = "Device is not uploading log to server for every 2 minutes aven after changing the log upload frequency to 2 minutes";
		    LOGGER.error(errorMessage);
		}

		LOGGER.info("S13 ACTUAL RESULT : " + (status
			? "SUCCESSFULLY VERIFIED LOG UPLOAD IS HAPPENING FOR EVERY 2 MINUTES USING WEBPA PARAMS !!!"
			: errorMessage));
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	    }

	    testStepNumber = "s14";
	    status = false;

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION:Configure the ping data block size using \"Device.IP.Diagnostics.IPPing.DataBlockSize\" TR-069/WebPA parameter and verify status");
	    LOGGER.info(
		    "STEP 14: ACTION: Set ping data block size as 100 by executing command \"Device.IP.Diagnostics.IPPing.DataBlockSize\" TR-069/WebPA parameter");
	    LOGGER.info("STEP 14: EXPECTED: Ping data block size must be changed to 100 ");
	    LOGGER.info(
		    "************************************************************************************************");

	    errorMessage = "Failed to configure ping data block size using \"Device.IP.Diagnostics.IPPing.DataBlockSize\" TR-069/WebPA parameter  ";
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_DATABLOCKSIZE, WebPaDataTypes.INTEGER.getValue(),
		    BroadBandTestConstants.STRING_VALUE_ONE_HUNDRED);

	    LOGGER.info("S14 ACTUAL RESULT : "
		    + (status ? "SUCCESSFULLY CHANGED PING DATA BLOCK SIZE UISNG WEBPA PARAMETER !!!" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION: Verify ping data block size using system commands ");
	    LOGGER.info(
		    "STEP 15: ACTION: Execute system command \"cat /opt/secure/data/syscfg.db | grep selfheal_ping_DataBlockSize\" and verify packet size");
	    LOGGER.info("STEP 15: EXPECTED: Ping data block size must be changed to 100");
	    LOGGER.info(
		    "************************************************************************************************");

	    testStepNumber = "s15";
	    status = false;

	    errorMessage = "System command response is not same as the webpa params response for ping data block size";
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_GET_PING_DATA_BLOCK_SIZE);
	    response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_GET_PING_DATA_BLOCK_SIZE);

	    if (CommonMethods.isNotNull(response)) {
		LOGGER.info("Obtained ping data block size using system commands : " + response);
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE_HUNDRED);
	    } else {
		errorMessage = "Not able to get the ping data block size using system command cat /opt/secure/data/syscfg.db ";
		LOGGER.error(errorMessage);
	    }

	    LOGGER.info("Expected ping data block size is : 100 and obtained ping data block size is : " + response);
	    LOGGER.info("S15 ACTUAL RESULT : "
		    + (status ? "SUCCESSFULLY VERIFIED PING DATA BLOCK SIZE UISNG SYSTEM COMMANDS !!!" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    errorMessage = "Exception occurred during execution : " + exception.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} finally {
	    // Disable diagnostic mode
	    BroadBandSelfHealUtils.enableOrDisableDiagnosticModeUsingWebpaParams(device, tapEnv,
		    BroadBandTestConstants.FALSE);
	    // set log upload frequency to default value
	    BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DIAG_MODE_LOG_UPLOAD_FREQUENCY,
		    WebPaDataTypes.INTEGER.getValue(), BroadBandTestConstants.STRING_VALUE_1440);
	    // Set ping data block size to default value
	    BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_DATABLOCKSIZE, WebPaDataTypes.INTEGER.getValue(),
		    BroadBandTestConstants.STRING_VALUE_ONE_HUNDRED);

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SELF-HEAL-4004");

    }

    /**
     * Test to verify the port forwarding and DMZ feature is working when firewall is configured to Maximum Security
     * 
     * <ol>
     * <li>STEP 1: Verify whether the firewall setting is configured to 'Maximum Security' for IPv4 traffic</li>
     * <li>STEP 2: Verify whether the port forwarding feature can be enabled using WebPA</li>
     * <li>STEP 3: Verify whether Private Wi-Fi SSID is enabled using WebPA</li>
     * <li>STEP 4: Connect the device to private Wi-Fi SSID and verify connection status</li>
     * <li>STEP 5: Verify the IP Address is retrieved successfully form connected client</li>
     * <li>STEP 6: Verify the port forwarding rule for HTTP service is added for the client connected to the gateway
     * </li>
     * <li>STEP 7: Verify the WAN IPv4 Address of the gateway is retrieved successfully</li>
     * <li>STEP 8: Verify the HTTP connection is successful when port forwarding is configured</li>
     * <li>STEP 9: Verify the port forwarding rule for HTTP service can be deleted successfully</li>
     * <li>STEP 10: Verify whether the port forwarding feature can be disabled using WebPA</li>
     * <li>STEP 11:Verify whether the DMZ feature can be enabled using WebPA</li>
     * <li>STEP 12: Configure the connected client LAN IPv4 Address to the DMZ</li>
     * <li>STEP 13: Verify the HTTP connection is successful when DMZ is configured</li>
     * <li>STEP 14: Verify whether the DMZ feature can be disabled using WebPA</li>
     * <li>STEP 15: Verify if the connected client can resolve the DNS queries when Firewall is configured to Maximum
     * Security</li>
     * <li>STEP 16: Verify if the connected client can able to access via gateway when Firewall is configured to Maximum
     * Security</li>
     * <li>POST-CONDITION: Verify whether the firewall setting is configured to 'Minimum Security' for IPv4 traffic</li>
     * </ol>
     * 
     * @param device
     * @Refactor Alan_Bivera
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-FIREWALL-1002")
    public void testToVerifyPortForwardingAndDmzInMaximumFirewall(Dut device) {

	String testId = "TC-RDKB-FIREWALL-102";
	int stepNumber = 1;
	String testStepNumber = "S" + stepNumber;
	String errorMessage = null;
	boolean status = false;
	Dut connectedClientSettop = null; // connected device to be verified
	BroadBandResultObject result = null; // stores test result and error message
	String portForwardingTableAddRowResponse = null;
	boolean isRuleDeleted = false;

	try {

	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-FIREWALL-1002 #####################");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify the port forwarding and DMZ feature is working when firewall is configured to Maximum Security");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1: Verify the firewall setting is configured to 'Maximum Security' for IPv4 traffic");
	    LOGGER.info("2: Verify the port forwarding feature can be enabled using WebPA");
	    LOGGER.info("3: Verify Private Wi-Fi SSID is enabled using WebPA");
	    LOGGER.info("4: Connect the device to private Wi-Fi SSID and verify connection status");
	    LOGGER.info("5: Verify the IP Address is retrieved successfully form connected client");
	    LOGGER.info(
		    "6: Verify the port forwarding rule for HTTP service is added for the client connected to the gateway");
	    LOGGER.info("7: Verify the WAN IPv4 Address of the gateway is retrieved successfully");
	    LOGGER.info("8: Verify the HTTP connection is successful when port forwarding is configured");
	    LOGGER.info("9: Verify the port forwarding rule for HTTP service can be deleted successfully");
	    LOGGER.info("10: Verify the port forwarding feature can be disabled using WebPA");
	    LOGGER.info("11: Verify whether the DMZ feature can be enabled using WebPA");
	    LOGGER.info("12: Configure the connected client LAN IPv4 Address to the DMZ");
	    LOGGER.info("13: Verify the HTTP connection is successful when DMZ is configured");
	    LOGGER.info("14: Verify whether the DMZ feature can be disabled using WebPA");
	    LOGGER.info(
		    "15: Verify if the connected client can resolve the DNS queries when Firewall is configured to Maximum Security");
	    LOGGER.info(
		    "16: Verify if the connected client can able to access via gateway when Firewall is configured to Maximum Security");
	    LOGGER.info(
		    "POST-CONDITION: Verify whether the firewall setting is configured to 'Minimum Security' for IPv4 traffic");
	    LOGGER.info("#####################################################################################");

	    /**
	     * Step 1: Verify the firewall setting is configured to 'Maximum Security' for IPv4 traffic
	     */
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the firewall setting is configured to 'Maximum Security' for IPv4 traffic");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: Firewall Setting for IPv4 traffic should be set to Custom Security");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL, WebPaDataTypes.STRING.getValue(),
		    BroadBandTestConstants.FIREWALL_IPV4_MAXIMUM_SECURITY);
	    errorMessage = "Firewall Setting for IPv4 traffic cannot be set to 'Maximum Security'";
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Firewall Setting for IPv4 traffic is successfully set to 'Maximum Security'");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 2: Verify the port forwarding feature can be enabled using WebPA
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the port forwarding feature can be enabled using WebPA");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: Port Forwarding feature should be enabled using WebPA");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_AND_DISABLE_PORT_FORWARDING,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    errorMessage = "Port Forwarding feature cannot be enabled using WebPA";
	    if (status) {
		LOGGER.info(testStepNumber + " ACTUAL: Port Forwarding feature is enabled successfully using WebPA");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 3: Verify whether Private Wi-Fi SSIDs are enabled using WebPA
	     *
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether Private Wi-Fi SSIDs are enabled using WebPA");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED: Both 2.4 GHz & 5 GHz private Wi-Fi radios should be enabled");
	    status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, true)
		    && BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
			    WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, true);
	    errorMessage = "Enabling private Wi-Fi SSIDs' via WebPA failed";
	    if (status) {
		LOGGER.info(
			testStepNumber + " ACTUAL: Both 2.4 GHz & 5 GHz Private Wi-Fi SSIDs' are enabled using WebPA");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 4: Connect the device to the private Wi-Fi SSID and verify connection status
	     * 
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Connect the device to the private Wi-Fi SSID and verify connection status");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should be connected to private Wi-Fi network");
	    try {
		errorMessage = "Unable to connect to 2.4 GHz private Wi-Fi Network";
		connectedClientSettop = BroadBandConnectedClientUtils.getWindowsClientsAndConnectToGivenSSID(device,
			tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    } catch (TestException e) {
		try {
		    errorMessage = "Unable to connect to both 2.4 GHz & 5 GHz private Wi-Fi Network";
		    connectedClientSettop = BroadBandConnectedClientUtils.getWindowsClientsAndConnectToGivenSSID(device,
			    tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
		} catch (TestException ex) {
		    errorMessage = ex.getMessage();
		}
	    }
	    status = null != connectedClientSettop;
	    String successMessage = null;
	    if (status) {
		successMessage = "Connected Client is assigned with a valid IPv4 Address DHCP Range";
		status = BroadBandConnectedClientUtils.verifyIISStatus(connectedClientSettop, tapEnv,
			BroadBandConnectedClientTestConstants.IIS_START_FLAG);
		successMessage = successMessage + " - IIS service is running successfully";
		errorMessage = successMessage + " - Unnable to start IIS service";
	    }
	    if (status) {
		LOGGER.info("STEP 5 ACTUAL: " + successMessage);
	    } else {
		LOGGER.error("STEP 5 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 5: Check if the wireless connected client has an IP address from the gateway
	     *
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Check if the wireless connected client has an IP address from the gateway");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : IP Address should be retrieved from the Wireless Connected device");
	    String ipAddressRetrievedFromClient = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
	    		device, connectedClientSettop);
	    LOGGER.info("IP ADDRESS ASSIGNED TO THE CONNECTED CLIENT FROM DHCP : " + ipAddressRetrievedFromClient);
	    errorMessage = "Unable to retrieve the IP Address form the cilent connected to the private Wi-Fi";
	    if (CommonMethods.isNotNull(ipAddressRetrievedFromClient)) {
		status = CommonMethods.isIpv4Address(ipAddressRetrievedFromClient);
		errorMessage = "Cilent connected to the private Wi-Fi haven't received valid IP Address from Gateway";
	    }
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Client connected to the private Wi-Fi network has got IP Address from Gateway :"
			+ ipAddressRetrievedFromClient);
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 6: Verify the port forwarding rule for HTTP service is added for the client connected to the gateway
	     *
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the port forwarding rule for HTTP service is added for the client connected to the gateway");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Port Forwarding rule for HTTP service should be added successfully");
	    portForwardingTableAddRowResponse = BroadBandCommonUtils.configurePortForwardingRule(tapEnv, device,
		    BroadBandTestConstants.TRUE, BroadBandTestConstants.HTTP_PORT_NUMBER,
		    BroadBandTestConstants.HTTP_PORT_NUMBER, BroadBandTestConstants.PROTOCOL_TCP_AND_UDP,
		    ipAddressRetrievedFromClient, BroadBandTestConstants.PORT_FORWARDING_RULE_DESCRIPTION_AS_HTTP);
	    errorMessage = "Null response obtained when configuring Port Forwarding Rule";
	    if (CommonMethods.isNotNull(portForwardingTableAddRowResponse)) {
		status = CommonUtils.patternSearchFromTargetString(portForwardingTableAddRowResponse,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_CONFIGURE_PORT_FORWARDING_RULE);
		errorMessage = "Port Forwarding rule for HTTP service cannot be configured";
	    }
	    if (status) {
		// Waiting for the rule to apply
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		LOGGER.info(
			testStepNumber + " ACTUAL: Port Forwarding rule for HTTP service is configured successfully");
	    } else {
		errorMessage = errorMessage + " ACTUAL RESPONSE: " + portForwardingTableAddRowResponse;
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 7: Check if the wireless connected client has an IP address from the gateway
	     *
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the WAN IP Address of the gateway is retrieved successfully");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : WAN IP Address of the gateway should be retieved successfully");
	    String wanIpAddress = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV4);
	    LOGGER.info("WAN IP ADDRESS OF THE DEVICE: " + wanIpAddress);
	    errorMessage = "Unable to retrieve WAN IP Address of the gateway using WebPA/dmcli. ACTUAL RESPONSE: "
		    + wanIpAddress;
	    status = CommonMethods.isNotNull(wanIpAddress) && CommonMethods.isIpv4Address(wanIpAddress);
	    if (status) {
		LOGGER.info(testStepNumber + " ACTUAL: WAN IP Address of the gateway is retieved successfully :"
			+ wanIpAddress);
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 8: Verify the HTTP connection is successful when portforwarding is configured
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the HTTP connection is successful when portforwarding is configured");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : HTTP connection to the host should be successful");
	    result = BroadBandCommonUtils.verifyConnectivityFromJumpServerUsingCurl(tapEnv, device, wanIpAddress);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: HTTP connection to the host is successful, Port Forwarding is working good when firewall is configured to 'Maximum Security'");
	    } else {
		errorMessage = CommonUtils.patternSearchFromTargetString(errorMessage, "IP Address is not accessible")
			? "HTTP connection to host failed even after configuring port forwarding, when firewall is set to 'Maximum Security'"
			: errorMessage;
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 9: Verify the port forwarding rule for HTTP service can be deleted successfully
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the port forwarding rule for HTTP service can be deleted successfully");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Port Forwarding rule for HTTP service should be deleted successfully");
	    WebPaServerResponse deleteResponse = tapEnv.deleteTableRowUsingRestApi(device,
		    portForwardingTableAddRowResponse);
	    errorMessage = "Null response obtained for deleting Port Forwarding Rule.";
	    if (CommonMethods.isNotNull(deleteResponse.getMessage())) {
		status = deleteResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		errorMessage = "Unable to delete the Port Forwarding rule configured for HTTP Service.";
	    }
	    isRuleDeleted = status;
	    if (status) {
		LOGGER.info(testStepNumber + " ACTUAL: Port Forwarding rule for HTTP service is deleted successfully");
	    } else {
		errorMessage = errorMessage + " ACTUAL RESPONSE: " + deleteResponse.getMessage();
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 10: Verify the port forwarding feature can be disabled using WebPA
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the port forwarding feature can be disabled using WebPA");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: Port Forwarding feature should be disabled using WebPA");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_AND_DISABLE_PORT_FORWARDING,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);
	    errorMessage = "Port Forwarding feature cannot be disabled using WebPA";
	    if (status) {
		LOGGER.info(testStepNumber + " ACTUAL: Port Forwarding feature is disabled successfully using WebPA");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 11: Verify whether the DMZ feature can be enabled using WebPA
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Verify whether the DMZ feature can be enabled using WebPA");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: DMZ feature should be enabled using WebPA");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_AND_DISABLE_DMZ, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE);
	    errorMessage = "DMZ feature cannot be enabled using WebPA";
	    if (status) {
		LOGGER.info(testStepNumber + " ACTUAL: DMZ feature is enabled successfully using WebPA");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 12: Configure the connected client LAN IPv4 Address to the DMZ
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Configure the connected client LAN IP Address to the DMZ");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: LAN IP Address of the connected client should be successfully configured to the DMZ");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_CONFIGURE_LAN_IP_ADDRESS_TO_DMZ,
		    WebPaDataTypes.STRING.getValue(), ipAddressRetrievedFromClient);
	    errorMessage = "LAN IP Address of the connected client cannot configured to the DMZ using WebPA";
	    if (status) {
		// Waiting for the rule to apply
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		LOGGER.info(testStepNumber
			+ " ACTUAL: LAN IP Address of the connected client is successfully configured to the DMZ");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 13: Verify the HTTP connection is successful when DMZ is configured
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the HTTP connection is successful when DMZ is configured");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : HTTP connection to the host should be successful");
	    result = BroadBandCommonUtils.verifyConnectivityFromJumpServerUsingCurl(tapEnv, device, wanIpAddress);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: HTTP connection to the host is successful, DMZ is working good when firewall is configured to 'Maximum Security'");
	    } else {
		errorMessage = CommonUtils.patternSearchFromTargetString(errorMessage, "IP Address is not accessible")
			? "HTTP connection to host failed even after configuring DMZ, when firewall is set to 'Maximum Security'"
			: errorMessage;
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 14: Verify whether the DMZ feature can be disabled using WebPA
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether the DMZ feature can be disabled using WebPA");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: DMZ feature should be disabled using WebPA");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_AND_DISABLE_DMZ, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.FALSE);
	    errorMessage = "DMZ feature cannot be disabled using WebPA";
	    if (status) {
		LOGGER.info(testStepNumber + " ACTUAL: DMZ feature is disabled successfully using WebPA");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 15: Verify if the connected client can resolve the DNS queries when Firewall is configured to
	     * Maximum Security
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify if the connected client can resolve the DNS queries when Firewall is configured to 'Maximum Security'");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: DNS resolution from Domain Name to IP Address must be successful from the connected client when Firewall is configured to 'Maximum Security'");
	    String nslookupResponse = tapEnv.executeCommandOnOneIPClients(connectedClientSettop,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.STRING_NS_LOOKUP,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK));
	    List<String> ipAddress = BroadBandCommonUtils.patternFinderForMultipleMatches(nslookupResponse,
		    BroadBandTestConstants.PATTERN_TO_GET_IPV4_ADDRESS, BroadBandTestConstants.CONSTANT_1);
	    status = ipAddress != null && !ipAddress.isEmpty() && ipAddress.size() >= BroadBandTestConstants.CONSTANT_1;
	    errorMessage = "Unable to retrieve IP address of facebook using nslookup. ACTUAL RESPONSE: "
		    + nslookupResponse;
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: DNS resolution from Domain Name to IP Address is successful from the connected client when Firewall is configured to 'Maximum Security'");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 16: Verify if the connected client can able to access Internet via gateway when Firewall is
	     * configured to 'Maximum Security'
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the connected client can able to access Internet via gateway when Firewall is configured to 'Maximum Security'");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Internet should be accessible from the connected client when Firewall is configured to Maximum Security");
	    result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
		    connectedClientSettop, BroadBandTestConstants.URL_WIKIPEDIA, BroadBandTestConstants.EMPTY_STRING);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(testStepNumber
			+ " ACTUAL: Internet is accessible from the connected client when Firewall is configured to Maximum Security");
	    } else {
		LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception testException) {
	    errorMessage = testException.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING PORT FORWARDING & DMZ FEATURE IN MAXIMUM FIREWALL : "
		    + errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} finally {
	    LOGGER.info("########################### STARTING POST-CONFIGURATIONS ###########################");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION: Verify whether the firewall setting is configured to 'Minimum Security' for IPv4 traffic");
	    LOGGER.info(
		    "POST-CONDITION : EXPECTED: Firewall Setting for IPv4 traffic should be set to 'Minimum Security'");
	    if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL, WebPaDataTypes.STRING.getValue(),
		    BroadBandTestConstants.FIREWALL_IPV4_MINIMUM_SECURITY)) {
		LOGGER.info(
			"POST-CONDITION PASSED: Firewall Setting for IPv4 traffic is set successfully to 'Minimum Security'");
	    } else {
		LOGGER.error(
			"POST-CONDITION FAILED: Firewall Setting for IPv4 traffic cannot be set to 'Minimum Security'");
	    }

	    if (!isRuleDeleted) {
		LOGGER.info("#####################################################################################");
		LOGGER.info(
			"POST-CONDITION : DESCRIPTION : Verify the port forwarding rule for HTTP service can be deleted successfully");
		LOGGER.info(
			"POST-CONDITION : EXPECTED : Port Forwarding rule for HTTP service should be deleted successfully");
		WebPaServerResponse deleteResponse = tapEnv.deleteTableRowUsingRestApi(device,
			portForwardingTableAddRowResponse);
		if (deleteResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT)) {
		    LOGGER.info("POST-CONDITION PASSED: Port Forwarding rule for HTTP service is deleted successfully");
		} else {
		    LOGGER.error(
			    "POST-CONDITION FAILED: Unable to delete the Port Forwarding rule configured for HTTP Service");
		}
	    }
	    LOGGER.info("########################### COMPLETED POST-CONFIGURATIONS ###########################");
	    LOGGER.info("ENDING TEST CASE: TC-RDKB-FIREWALL-1002");
	}
    }

}
