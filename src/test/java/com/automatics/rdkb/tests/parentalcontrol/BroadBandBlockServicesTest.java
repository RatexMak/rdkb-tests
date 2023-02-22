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
package com.automatics.rdkb.tests.parentalcontrol;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.parentalcontrol.BroadBandParentalControlUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaServerResponse;

public class BroadBandBlockServicesTest extends AutomaticsTestBase {

	/**
	 * Test to Verify the FTP port can be blocked in Parental Control - Managed
	 * Services
	 * <ol>
	 * <li>STEP 1: Verify Parental Control - Managed Services can be enabled via
	 * WebPA</li>
	 * <li>STEP 2: Verify the rule to block FTP Services can be added in Parental
	 * Control - Managed Services via WebPA</li>
	 * <li>STEP 3: Verify the rule added to block FTP Services is listed in
	 * iptables</li>
	 * <li>STEP 4: Verify the rule added to block FTP Services is listed in
	 * ip6tables</li>
	 * <li>STEP 5: Verify whether Private 2.4 GHz and 5 GHz SSIDs' are enabled using
	 * WebPA</li>
	 * <li>STEP 6: Connect the device to 2.4 GHz SSID and verify connection
	 * status</li>
	 * <li>STEP 7: Check if the wireless connected client has an IP address from the
	 * gateway</li>
	 * <li>STEP 8: Verify the FTP Service is blocked from the client connected to
	 * 2.4 GHz Private Wi-Fi Network</li>
	 * <li>STEP 9: Verify HTTP/HTTPS Service sites can be accessible from the client
	 * connected to 2.4 GHz Private Wi-Fi Network</li>
	 * <li>STEP 10: Connect the device to 5 GHz SSID and verify connection
	 * status</li>
	 * <li>STEP 11: Check if the wireless connected client has an IP address from
	 * the gateway</li>
	 * <li>STEP 12: Verify the FTP Service is blocked from the client connected to 5
	 * GHz Private Wi-Fi Network</li>
	 * <li>STEP 13: Verify HTTP/HTTPS Service sites can be accessible from the
	 * client connected to 5 GHz Private Wi-Fi Network</li>
	 * <li>STEP 14: Verify the FTP Service is blocked from the client connected to
	 * Ethernet</li>
	 * <li>STEP 15: Verify HTTP/HTTPS Service sites can be accessible from the
	 * client connected to Ethernet</li>
	 * <li>POST-CONDITION 1: Verify the Parental Control - Managed Service rule to
	 * block FTP is removed</li>
	 * <li>POST-CONDITION 2: Verify the Parental Control - Managed Services is
	 * disabled via WebPA</li>
	 * </ol>
	 * 
	 * @param device
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-PC-MNG-SERVICES-1000")
	public void testToBlockFTPServiceAndCheckInternetAccessToFTPSites(Dut device) {

		String testId = "TC-RDKB-PC-MNG-SERVICES-100";
		String testStepNumber = "s1";
		String errorMessage = null;
		boolean status = false;
		boolean isEnabled = false;
		Dut connectedClientSettop = null; // connected device to be verified
		BroadBandResultObject result = null; // stores test result and error
		// message
		String parentalControlManageServiceRuleAddRowResponse = null;
		String ipTableResponse = null;
		String ip6TableResponse = null;

		try {

			LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-PC-MNG-SERVICES-1000 #####################");
			LOGGER.info("TEST DESCRIPTION: Verify the FTP port can be blocked in Parental Control - Managed Services");

			LOGGER.info("TEST STEPS : ");
			LOGGER.info("1: Verify Parental Control -  Managed Services can be enabled via WebPA");
			LOGGER.info(
					"2: Verify the rule to block FTP Services can be added in Parental Control -  Managed Services via WebPA");
			LOGGER.info("3: Verify the rule added to block FTP Services is listed in iptables");
			LOGGER.info("4: Verify the rule added to block FTP Services is listed in ip6tables");
			LOGGER.info("5: Verify whether Private 2.4 GHz and 5 GHz SSIDs' are enabled using WebPA");
			LOGGER.info("6: Connect the device to 2.4 GHz SSID and verify connection status");
			LOGGER.info("7: Check if the wireless connected client has an IP address from the gateway");
			LOGGER.info(
					"8: Verify the FTP Service is blocked from the client connected to 2.4 GHz Private Wi-Fi Network");
			LOGGER.info(
					"9: Verify HTTP/HTTPS Service sites can be accessible from the client connected to 2.4 GHz Private Wi-Fi Network");
			LOGGER.info("10: Connect the device to 5 GHz SSID and verify connection status");
			LOGGER.info("11: Check if the wireless connected client has an IP address from the gateway");
			LOGGER.info(
					"12: Verify the FTP Service is blocked from the client connected to 5 GHz Private Wi-Fi Network");
			LOGGER.info(
					"13: Verify HTTP/HTTPS Service sites can be accessible from the client connected to 5 GHz Private Wi-Fi Network");
			LOGGER.info("14: Verify the FTP Service is blocked from the client connected to Ethernet");
			LOGGER.info("15: Verify HTTP/HTTPS Service sites can be accessible from the client connected to Ethernet");
			LOGGER.info("POST-CONDITION 1: Verify the Parental Control - Managed Service rule to block FTP is removed");
			LOGGER.info("POST-CONDITION 2: Verify the Parental Control - Managed Services is disabled via WebPA");
			LOGGER.info("#####################################################################################");

			/**
			 * Step 1: Verify Parental Control - Managed Services can be enabled via WebPA
			 *
			 */
			testStepNumber = "s1";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 1: Verify Parental Control -  Managed Services can be enabled via WebPA");
			LOGGER.info("STEP 1: EXPECTED: Parental Control -  Managed Services should be enabled via WebPA");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_PARENTAL_CONTROLS_MANAGED_SERVICES,
					WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
			isEnabled = status;
			errorMessage = "Parental Control -  Managed Services cannot be enabled";
			if (status) {
				LOGGER.info("S1 ACTUAL: Parental Control -  Managed Services is enabled via WebPA");
			} else {
				LOGGER.error("S1 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 2: Verify the rule to block FTP Services can be added in Parental
			 * Control - Managed Services via WebPA
			 *
			 */
			testStepNumber = "s2";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 2: Verify the rule to block FTP Services can be added in Parental Control -  Managed Services via WebPA");
			LOGGER.info(
					"STEP 2: EXPECTED: Rule to block FTP port should be added to Parental Control -  Managed Services via WebPA");

			// added addRuleToParentalControlManagedServicesviaWebpa method as replacement

			parentalControlManageServiceRuleAddRowResponse = BroadBandParentalControlUtils
					.addRuleToParentalControlManagedServicesviaWebpa(tapEnv, device,
							BroadBandTestConstants.MNG_SERVICES_DESCRIPTION_AS_FTP,
							BroadBandTestConstants.PROTOCOL_TCP_AND_UDP, BroadBandTestConstants.FTP_PORT_NUMBER,
							BroadBandTestConstants.FTP_PORT_NUMBER, BroadBandTestConstants.TRUE);

			errorMessage = "Null response obtained for setting Parental Control - Managed Services Rule";
			if (CommonMethods.isNotNull(parentalControlManageServiceRuleAddRowResponse)) {
				status = CommonUtils.patternSearchFromTargetString(parentalControlManageServiceRuleAddRowResponse,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_ADD_A_PARENTAL_CONTROL_MANAGED_SERVICES_RULE);
				errorMessage = "Rule to block FTP port to Parental Control -  Managed Services via WebPA cannot be added";
			}
			if (status) {
				LOGGER.info(
						"S2 ACTUAL: Rule to block FTP port to Parental Control -  Managed Services via WebPA has been added successfully for all days");
			} else {
				LOGGER.error("S2 ACTUAL: " + errorMessage + " ACTUAL RESPONSE: "
						+ parentalControlManageServiceRuleAddRowResponse);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 3: Verify the rule added to block FTP Services is listed in iptables
			 *
			 */
			testStepNumber = "s3";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 3: Verify the rule added to block FTP Services is listed in iptables");
			LOGGER.info("STEP 3: EXPECTED: FTP Port Number should be listed in iptables as ServiceBlocked");
			LOGGER.info("################## Waiting for 30 seconds to reflect the changes ####################");
			// wait to log in iptables
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			ipTableResponse = tapEnv.executeCommandUsingSsh(device,
					BroadBandCommonUtils.concatStringUsingStringBuffer(
							BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IPTABLE,
							BroadBandTraceConstants.LOG_MESSAGE_CHECK_SERVICE_BLOCKED_IN_IPTABLES));
			errorMessage = "Null response received on obtaining values from iptables.";
			if (CommonMethods.isNotNull(ipTableResponse)) {
				status = CommonUtils.patternSearchFromTargetString(ipTableResponse,
						BroadBandTraceConstants.LOG_MESSAGE_CHECK_SERVICE_BLOCKED_IN_IPTABLES)
						&& CommonUtils.patternSearchFromTargetString(ipTableResponse,
								BroadBandTraceConstants.LOG_MESSAGE_CHECK_FTP_SERVICE_BLOCKED);
				errorMessage = "FTP Port Number is not listed in iptables.";
			}
			if (status) {
				LOGGER.info("S3 ACTUAL: FTP Port Number is listed in iptables as ServiceBlocked");
			} else {
				LOGGER.error("S3 ACTUAL: " + errorMessage + " ACTUAL RESPONSE: " + ipTableResponse);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 4: Verify the rule added to block FTP Services is listed in ip6tables
			 *
			 */
			testStepNumber = "s4";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 4: Verify the rule added to block FTP Services is listed in ip6tables");
			LOGGER.info("STEP 4: EXPECTED: FTP Port Number should be listed in ip6tables as ServiceBlocked");
			LOGGER.info("################## Waiting for 30 seconds to reflect the changes ####################");
			// wait to log in ip6tables
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			ip6TableResponse = tapEnv.executeCommandUsingSsh(device,
					BroadBandCommonUtils.concatStringUsingStringBuffer(
							BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IP6TABLES,
							BroadBandTraceConstants.LOG_MESSAGE_CHECK_SERVICE_BLOCKED_IN_IPTABLES));
			errorMessage = "Null response received on obtaining values from ip6tables.";
			if (CommonMethods.isNotNull(ip6TableResponse)) {
				status = CommonUtils.patternSearchFromTargetString(ip6TableResponse,
						BroadBandTraceConstants.LOG_MESSAGE_CHECK_SERVICE_BLOCKED_IN_IPTABLES)
						&& CommonUtils.patternSearchFromTargetString(ip6TableResponse,
								BroadBandTraceConstants.LOG_MESSAGE_CHECK_FTP_SERVICE_BLOCKED);
				errorMessage = "FTP Port Number is not listed in ip6tables.";
			}
			if (status) {
				LOGGER.info("S4 ACTUAL: FTP Port Number is listed in ip6tables as ServiceBlocked");
			} else {
				LOGGER.error("S4 ACTUAL: " + errorMessage + " ACTUAL RESPONSE: " + ip6TableResponse);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 5: Verify whether Private 2.4 GHz & 5 GHz SSIDs' are enabled using WebPA
			 *
			 */
			testStepNumber = "s5";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 5: Verify whether Private 2.4 GHz & 5 GHz SSIDs' are enabled using WebPA");
			LOGGER.info("STEP 5: EXPECTED: Both 2.4 GHz & 5 GHz private Wi-Fi radios should be enabled");
			status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
					WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, true)
					&& BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
							WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, true);
			errorMessage = "Enabling private Wi-Fi SSIDs' via WebPA failed";
			if (status) {
				LOGGER.info("S5 ACTUAL: Both 2.4 GHz & 5 GHz Private Wi-Fi SSIDs' are enabled using WebPA");
			} else {
				LOGGER.error("S5 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 6: Connect the device to 2.4 GHz SSID and verify connection status
			 * 
			 */
			testStepNumber = "s6";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 6: Connect the device to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP 6: EXPECTED: Device should be connected with 2.4 GHz wifi network");
			connectedClientSettop = BroadBandConnectedClientUtils
					.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			status = null != connectedClientSettop;
			errorMessage = "Unable to connect to 2.4 GHz private Wi-Fi Network Or 2.4 GHz WiFi capable devices are not available";
			if (status) {
				LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				LOGGER.info("S6 ACTUAL: Device has been connected with 2.4 GHz private Wi-Fi network");
			} else {
				LOGGER.error("S6 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 7: Check if the wireless connected client has an IP address from the
			 * gateway
			 *
			 */
			testStepNumber = "s7";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 7: Check if the wireless connected client has an IP address from the gateway");
			LOGGER.info(
					"STEP 7: EXPECTED: DHCP Range IP Address should be assigned to 2.4 GHz Wireless Connected device");
			status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
					connectedClientSettop);
			errorMessage = "Cilent connected to 2.4 GHz private Wi-Fi haven't received valid IP Address from Gateway";
			if (status) {
				LOGGER.info(
						"S7 ACTUAL: Client connected to 2.4 Ghz private Wi-Fi network has got IP Address from Gateway");
			} else {
				LOGGER.error("S7 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 8: Verify the FTP Service is blocked from the client connected to 2.4
			 * GHz Private Wi-Fi Network
			 *
			 */
			testStepNumber = "s8";
			status = false;
			if (!DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info("#####################################################################################");
				LOGGER.info(
						"STEP 8: Verify the FTP Service is blocked from the client connected to 2.4 GHz Private Wi-Fi Network");
				LOGGER.info(
						"STEP 8: EXPECTED: Connection to the ftp host should fail from the client connected to 2.4 GHz Private Wi-Fi Network");
				result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurl(tapEnv, connectedClientSettop,
						BroadBandTestConstants.URL_FTP_HP);
			} else {
				result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurlForRPi(tapEnv, device,
						connectedClientSettop, BroadBandTestConstants.URL_FTP_HP);
			}
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info(
						"S8 ACTUAL: Connection to the ftp host is failed from the client connected to 2.4 GHz Private Wi-Fi Network");
			} else {
				LOGGER.error("S8 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 9: Verify HTTP/HTTPS Service sites can be accessible from the client
			 * connected to 2.4 GHz Private Wi-Fi Network
			 *
			 */
			testStepNumber = "s9";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 9: Verify HTTP/HTTPS Service sites can be accessible from the client connected to 2.4 GHz Private Wi-Fi Network");
			LOGGER.info(
					"STEP 9: EXPECTED:  HTTP/HTTPS Service sites should be accessible from the client connected to 2.4 GHz Private Wi-Fi Network");
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					connectedClientSettop, BroadBandTestConstants.URL_WIKIPEDIA, BroadBandTestConstants.EMPTY_STRING);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info(
						"S9 ACTUAL: HTTP/HTTPS Service site is accessible from the client connected to 2.4 GHz Private Wi-Fi Network");
			} else {
				LOGGER.error("S9 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 10: Connect the device to 5 GHz SSID and verify connection status
			 * 
			 */
			testStepNumber = "s10";
			status = false;
			connectedClientSettop = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 10: Connect the device to 5 GHz SSID and verify connection status");
			LOGGER.info("STEP 10: EXPECTED: Device should be connected with 5 GHz wifi network");
			connectedClientSettop = BroadBandConnectedClientUtils
					.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			status = null != connectedClientSettop;
			errorMessage = "Unable to connect to 5 GHz private Wi-Fi Network Or 5 GHz WiFi capable devices are not available";
			if (status) {
				LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				LOGGER.info("S10 ACTUAL: Device has been connected with 5 GHz private Wi-Fi network");
			} else {
				LOGGER.error("S10 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 11: Check if the wireless connected client has an IP address from the
			 * gateway
			 *
			 */
			testStepNumber = "s11";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 11: Check if the wireless connected client has an IP address from the gateway");
			LOGGER.info(
					"STEP 11: EXPECTED: DHCP Range IP Address should be assigned to 5 GHz Wireless Connected device");
			status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
					connectedClientSettop);
			errorMessage = "Cilent connected to 5 GHz private Wi-Fi haven't received valid IP Address from Gateway";
			if (status) {
				LOGGER.info(
						"S11 ACTUAL: Client connected to 5 Ghz private Wi-Fi network has got IP Address from Gateway");
			} else {
				LOGGER.error("S11 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 12: Verify the FTP Service is blocked from the client connected to 5 GHz
			 * Private Wi-Fi Network
			 *
			 */
			testStepNumber = "s12";
			status = false;
			if (!DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info("#####################################################################################");
				LOGGER.info(
						"STEP 12: Verify the FTP Service is blocked from the client connected to 5 GHz Private Wi-Fi Network");
				LOGGER.info(
						"STEP 12: EXPECTED: Connection to the ftp host should fail from the client connected to 5 GHz Private Wi-Fi Network");
				result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurl(tapEnv, connectedClientSettop,
						BroadBandTestConstants.URL_FTP_HP);
			} else {
				result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurlForRPi(tapEnv, device,
						connectedClientSettop, BroadBandTestConstants.URL_FTP_HP);
			}
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info(
						"S12 ACTUAL: Connection to the ftp host is failed from the client connected to 5 GHz Private Wi-Fi Network");
			} else {
				LOGGER.error("S12 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 13: Verify HTTP/HTTPS Service sites can be accessible from the client
			 * connected to 5 GHz Private Wi-Fi Network
			 *
			 */
			testStepNumber = "s13";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 13: Verify HTTP/HTTPS Service sites can be accessible from the client connected to 5 GHz Private Wi-Fi Network");
			LOGGER.info(
					"STEP 13: EXPECTED:  HTTP/HTTPS Service sites should be accessible from the client connected to 5 GHz Private Wi-Fi Network");
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					connectedClientSettop, BroadBandTestConstants.URL_WIKIPEDIA, BroadBandTestConstants.EMPTY_STRING);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info(
						"S13 ACTUAL: HTTP/HTTPS Service site is accessible from the client connected to 5 GHz Private Wi-Fi Network");
			} else {
				LOGGER.error("S13 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 14: Verify the FTP Service is blocked from the client connected to
			 * Ethernet
			 *
			 */
			testStepNumber = "s14";
			status = false;
			if (!DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info("#####################################################################################");
				LOGGER.info("STEP 14: Verify the FTP Service is blocked from the client connected to to Ethernet");
				LOGGER.info(
						"STEP 14: EXPECTED: Connection to the ftp host should fail from the client connected to Ethernet");
				connectedClientSettop = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
				result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurl(tapEnv, connectedClientSettop,
						BroadBandTestConstants.URL_FTP_HP);
				status = result.isStatus();
				errorMessage = result.getErrorMessage();
				if (status) {
					LOGGER.info(
							"S14 ACTUAL: Connection to the ftp host is failed from the client connected to Ethernet");
				} else {
					LOGGER.error("S14 ACTUAL: " + errorMessage);
				}
				LOGGER.info("#####################################################################################");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("RPi device setup dependency : skipping teststep...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * Step 15: Verify HTTP/HTTPS Service sites can be accessible from the client
			 * connected to Ethernet
			 *
			 */
			testStepNumber = "s15";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 15: Verify HTTP/HTTPS Service sites can be accessible from the client connected to Ethernet");
			LOGGER.info(
					"STEP 15: EXPECTED:  HTTP/HTTPS Service sites should be accessible from the client connected to Ethernet");
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					connectedClientSettop, BroadBandTestConstants.URL_WIKIPEDIA, BroadBandTestConstants.EMPTY_STRING);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("S15 ACTUAL: HTTP/HTTPS Service site is accessible from the client connected to Ethernet");
			} else {
				LOGGER.error("S15 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		} catch (Exception testException) {
			errorMessage = testException.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE BLOCKING FTP PORT IN PARENTAL CONTROL - MANAGED SERVICES: "
					+ errorMessage);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
		} finally {
			if (CommonMethods.isNotNull(parentalControlManageServiceRuleAddRowResponse)) {
				LOGGER.info("########################### STARTING POST-CONFIGURATIONS ###########################");
				LOGGER.info(
						"POST-CONDITION-1: DESCRIPTION: Verify the Parental Control - Managed Service rule to block FTP is removed");
				LOGGER.info(
						"POST-CONDITION-1: EXPECTED: Parental Control - Managed Service rule to block FTP should be removed");
				WebPaServerResponse webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device,
						parentalControlManageServiceRuleAddRowResponse);
				if (webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT)) {
					LOGGER.info(
							"POST-CONDITION-1 PASSED: Parental Control - Managed Service rule to block FTP has been successfully removed");
				} else {
					LOGGER.error(
							"POST-CONDITION-1 FAILED: Parental Control - Managed Service rule to block FTP cannot be removed");
				}

				LOGGER.info("########################### COMPLETED POST-CONFIGURATIONS ###########################");
			}

			if (isEnabled) {
				LOGGER.info("########################### STARTING POST-CONFIGURATIONS ###########################");
				LOGGER.info(
						"POST-CONDITION-2: DESCRIPTION: Verify the Parental Control - Managed Services is disabled via WebPA");
				LOGGER.info(
						"POST-CONDITION-2: EXPECTED: Parental Control -  Managed Services should be disabled via WebPA");
				if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_PARENTAL_CONTROLS_MANAGED_SERVICES,
						WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE)) {
					LOGGER.error(
							"POST-CONDITION-2 FAILED: Parental Control -  Managed Services cannot be disabled via WebPa");
				} else {
					LOGGER.info(
							"POST-CONDITION-2 PASSED: Parental Control - Managed Services has been successfully disabled via WebPA");
				}
				LOGGER.info("########################### COMPLETED POST-CONFIGURATIONS ###########################");
			}

		}
		LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-PC-MNG-SERVICES-1000 #####################");

	}

}