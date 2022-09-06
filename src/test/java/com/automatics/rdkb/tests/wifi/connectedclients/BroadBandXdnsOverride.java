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

package com.automatics.rdkb.tests.wifi.connectedclients;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.dmcli.DmcliUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.utils.AutomaticsPropertyUtility;

public class BroadBandXdnsOverride extends AutomaticsTestBase {

	/** SLF4j logger. */
	public static final Logger LOGGER = LoggerFactory.getLogger(BroadBandXdnsOverride.class);

	/** Constant holds the test step number with S **/
	private static String stepNum = null;

	/** Constant holds the test status **/
	private static boolean status = false;

	/** Constant holds the errormessage **/
	private static String errorMessage = null;

	/**
	 * 
	 * <ol>
	 * <li>1 :Connect to a connected client with 2.4Ghz/5Ghz radio ssid and verify
	 * connection status</li>
	 * <li>2 :Execute nslookup facebook.com Wifi connected client and verify
	 * output</li>
	 * <li>3 :Execute nslookup gmail.com 8.8.8.8 Wifi connected client and verify
	 * output</li>
	 * <li>4 :Execute nslookup amazon.com 75.75.76.76 Wifi connected client and
	 * verify output</li>
	 * <li>5 :Execute nslookup opendns.com 208.67.222.222 Wifi connected client and
	 * verify output</li>
	 * <li>6 :Execute nslookup yahoo.com 208.67.220.220 Wifi connected client and
	 * verify output</li>
	 * <li>7 :Execute nslookup comcast.com 208.67.222.220 Wifi connected client and
	 * verify output</li>
	 * <li>8 :Execute nslookup cisco.com 208.67.220.222 Wifi connected client and
	 * verify output</li>
	 * <li>9 :Retrieve ethernet client from connected clients</li>
	 * <li>10 :Execute nslookup facebook.com in ethernet connected client and verify
	 * output</li>
	 * <li>11 :Execute nslookup gmail.com 8.8.8.8 ethernet connected client and
	 * verify output</li>
	 * <li>12 :Execute nslookup amazon.com 75.75.76.76 ethernet connected client and
	 * verify output</li>
	 * <li>13 :Execute nslookup opendns.com 208.67.222.222 ethernet connected client
	 * and verify output</li>
	 * <li>14 :Execute nslookup yahoo.com 208.67.220.220 ethernet connected client
	 * and verify output</li>
	 * <li>15 :Execute nslookup comcast.com 208.67.222.220 ethernet connected client
	 * and verify output</li>
	 * <li>16 :Execute nslookup cisco.com 208.67.220.222 ethernet connected client
	 * and verify output</li>
	 * </ol>
	 * 
	 * @author PRASANTH REDDY
	 * @Refactor Alan_Bivera
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-ALTN-DNS-1001")
	public void testToVerifyAlternateDns(Dut device) {
		boolean status = false;// String to store the test case status
		String testId = "TC-RDKB-ALTN-DNS-101";// Test case id
		String testStep = null;// Test step number
		String errorMessage = null;// String to store the error message
		Dut wifiConnectedClient = null;
		String openDnsIP = null; // String to store dns ip
		try {
			LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-ALTN-DNS-1001#####################");
			LOGGER.info("TEST DESCRIPTION: Test for Alternate DNS in WiFi and Lan Connected clients");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("PRE-CONDITION 1: Verify whether Private SSID 2.4Ghz can be enabled using webPA");
			LOGGER.info("PRE-CONDITION 2: Verify whether Private SSID 5Ghz can be enabled using webPA");
			LOGGER.info("1.Connect to a connected client with 2.4Ghz/5Ghz radio ssid and verify connection status");
			LOGGER.info("2.Execute nslookup facebook.com Wifi connected client and verify output");
			LOGGER.info("3.Execute nslookup gmail.com 8.8.8.8 Wifi connected client and verify output");
			LOGGER.info("4.Execute nslookup amazon.com 75.75.76.76 Wifi connected client and verify output");
			LOGGER.info("5.Execute nslookup opendns.com 208.67.222.222 Wifi connected client and verify output");
			LOGGER.info("6.Execute nslookup yahoo.com 208.67.220.220 Wifi connected client and verify output");
			LOGGER.info("7.Execute nslookup comcast.com 208.67.222.220 Wifi connected client and verify output");
			LOGGER.info("8.Execute nslookup cisco.com 208.67.220.222 Wifi connected client and verify output");
			LOGGER.info("9.Retrieve ethernet client from connected clients");
			LOGGER.info("10.Execute nslookup facebook.com  in ethernet connected client and verify output");
			LOGGER.info("11.Execute nslookup gmail.com 8.8.8.8 ethernet connected client and verify output");
			LOGGER.info("12.Execute nslookup amazon.com 75.75.76.76 ethernet connected client and verify output");
			LOGGER.info("13.Execute nslookup opendns.com 208.67.222.222 ethernet connected client and verify output");
			LOGGER.info("14.Execute nslookup yahoo.com 208.67.220.220 ethernet connected client and verify output");
			LOGGER.info("15.Execute nslookup comcast.com 208.67.222.220 ethernet connected client and verify output");
			LOGGER.info("16.Execute nslookup cisco.com 208.67.220.222 ethernet connected client and verify output");
			LOGGER.info("#####################################################################################");
			LOGGER.info("############################# STARTING PRE-CONFIGURATIONS ##################################");
			LOGGER.info("PRE-CONFIGURATIONS TEST STEPS : ");
			LOGGER.info("1.Verify whether Private SSID 2.4Ghz can be enabled using webPA");
			LOGGER.info("2.Verify whether Private SSID 5Ghz can be enabled using webPA");
			LOGGER.info("#####################################################################################");
			LOGGER.info("PRE-CONDITION 1: VERIFY WHETHER PRIVATE SSID 2.4GHZ CAN BE ENABLED USING WEBPA");
			LOGGER.info("PRE-CONDITION 1: EXPECTED: PRIVATE SSID 2.4GHZ SHOULD BE ENABLED SUCCESSFULLY");
			errorMessage = "Unable to enable 2.4Ghz private SSID using webpa";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (!status) {
				throw new Exception(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			LOGGER.info("PRE-CONDITION 2: VERIFY WHETHER PRIVATE SSID 5GHZ CAN BE ENABLED USING WEBPA");
			LOGGER.info("PRE-CONDITION 2: EXPECTED: PRIVATE SSID 5GHZ SHOULD BE ENABLED SUCCESSFULLY");
			errorMessage = "Unable to enable 5Ghz private SSID using webpa";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (!status) {
				throw new Exception(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			/**
			 * STEP 1:Connect to a connected client with 2.4Ghz/5Ghz radio ssid and verify
			 * connection status
			 */
			status = false;
			testStep = "s1";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 1 : DESCRIPTION :CONNECT TO A CONNECTED CLIENT WITH 2.4GHZ/5GHZ RADIO SSID AND VERIFY CONNECTION STATUS");
			LOGGER.info(
					"STEP 1 : ACTION :CONNECTION ESTABLISHMENT BETWEEN CONNECTED CLIENT -2.4GHZ/5GHZ AND DEVICE SHOULD BE SUCCESSFUL");
			LOGGER.info("STEP 1 : EXPECTED:THE CONNECTION SHOULD BE SUCCESSFUL TO 2.4GHZ/5GHZ RADIO SSID");
			errorMessage = "Unable to establish wifi connection in connected client";
			try {
				wifiConnectedClient = BroadBandConnectedClientUtils
						.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			} catch (Exception e) {
				LOGGER.error("Exception in retrieving wifi connected client");
			}
			status = wifiConnectedClient != null;
			if (status) {
				LOGGER.info("STEP 1:ACTUAL :Connected to Wifi SSID successfully");
			} else {
				LOGGER.error("STEP 1:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
			/**
			 * STEP 2:Execute nslookup facebook.com Wifi connected client and verify output
			 */
			status = false;
			testStep = "s2";
			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 2 : DESCRIPTION :EXECUTE nslookup facebook.com WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info("STEP 2 : ACTION :EXECUTE COMMAND 'nslookup facebook.com' IN WIFI CONNECTED CLIENT");
			LOGGER.info("STEP 2 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK, "",
					BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK);
			if (status) {
				LOGGER.info("STEP 2:ACTUAL :Nslookup validation is successful");
			} else {
				LOGGER.error("STEP 2:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 3:Execute nslookup gmail.com 8.8.8.8 Wifi connected client and verify
			 * output
			 */
			status = false;
			testStep = "s3";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 3 : DESCRIPTION :EXECUTE nslookup gmail.com 8.8.8.8 WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info("STEP 3 : ACTION :EXECUTE COMMAND 'nslookup gmail.com 8.8.8.8' IN WIFI CONNECTED CLIENT");
			LOGGER.info("STEP 3 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS 8.8.8.8 response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_GMAIL,
					BroadBandTestConstants.STRING_OPEN_DNS_IP_8, BroadBandTestConstants.NSLOOKUP_FOR_GMAIL);
			if (status) {
				LOGGER.info("STEP 3:ACTUAL :Nslookup validation is successful with DNS 8.8.8.8");
			} else {
				LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 4:Execute nslookup amazon.com 75.75.76.76 Wifi connected client and
			 * verify output
			 */
			status = false;
			testStep = "s4";
			openDnsIP = DeviceModeHandler.isDSLDevice(device) ? BroadBandTestConstants.STRING_OPEN_DNS_IP_DSL
					: BroadBandTestConstants.STRING_OPEN_DNS_IP_75_76;
			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 4 : DESCRIPTION :EXECUTE nslookup amazon.com " + openDnsIP
					+ " WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info("STEP 4 : ACTION :EXECUTE COMMAND 'nslookup amazon.com " + openDnsIP
					+ " ' IN WIFI CONNECTED CLIENT");
			LOGGER.info("STEP 4 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS " + openDnsIP + " response in wifi connected client";

			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_AMAZON, openDnsIP,
					BroadBandTestConstants.NSLOOKUP_FOR_AMAZON);
			if (status) {
				LOGGER.info("STEP 4:ACTUAL :Nslookup validation is successful with DNS 75.75.76.76");
			} else {
				LOGGER.error("STEP 4:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 5:Execute nslookup opendns.com 208.67.222.222 Wifi connected client and
			 * verify output
			 */
			status = false;
			testStep = "s5";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 5 : DESCRIPTION :EXECUTE nslookup opendns.com 208.67.222.222 WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info(
					"STEP 5 : ACTION :EXECUTE COMMAND 'nslookup opendns.com 208.67.222.222' IN WIFI CONNECTED CLIENT");
			LOGGER.info("STEP 5 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS 208.67.222.222 response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_OPENDNS,
					BroadBandTestConstants.STRING_OPEN_DNS_IP_222, BroadBandTestConstants.NSLOOKUP_FOR_OPENDNS);
			if (status) {
				LOGGER.info("STEP 5:ACTUAL :Nslookup validation is successful with DNS 208.67.222.222");
			} else {
				LOGGER.error("STEP 5:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 6:Execute nslookup yahoo.com 208.67.220.220 Wifi connected client and
			 * verify output
			 */
			status = false;
			testStep = "s6";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 6 : DESCRIPTION :EXECUTE nslookup yahoo.com 208.67.220.220 WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info(
					"STEP 6 : ACTION :EXECUTE COMMAND 'nslookup yahoo.com 208.67.220.220' IN WIFI CONNECTED CLIENT");
			LOGGER.info("STEP 6 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS 208.67.220.220 response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_YAHOO,
					BroadBandTestConstants.STRING_OPEN_DNS_IP_220, BroadBandTestConstants.NSLOOKUP_FOR_YAHOO);
			if (status) {
				LOGGER.info("STEP 6:ACTUAL :Nslookup validation is successful with DNS 208.67.220.220");
			} else {
				LOGGER.error("STEP 6:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 7:Execute nslookup comcast.com 208.67.222.220 Wifi connected client and
			 * verify output
			 */
			status = false;
			testStep = "s7";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 7 : DESCRIPTION :EXECUTE nslookup comcast.com 208.67.222.220 WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info(
					"STEP 7 : ACTION :EXECUTE COMMAND 'nslookup comcast.com 208.67.222.220' IN WIFI CONNECTED CLIENT");
			LOGGER.info("STEP 7 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS 208.67.222.220 response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_COMCAST,
					BroadBandTestConstants.STRING_OPEN_DNS_IP_222_220, BroadBandTestConstants.NSLOOKUP_FOR_COMCAST);
			if (status) {
				LOGGER.info("STEP 7:ACTUAL :Nslookup validation is successful with DNS 208.67.222.220");
			} else {
				LOGGER.error("STEP 7:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 8:Execute nslookup cisco.com 208.67.220.222 Wifi connected client and
			 * verify output
			 */
			status = false;
			testStep = "s8";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 8 : DESCRIPTION :EXECUTE nslookup cisco.com 208.67.220.222 WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info(
					"STEP 8 : ACTION :EXECUTE COMMAND 'nslookup cisco.com 208.67.220.222' IN WIFI CONNECTED CLIENT");
			LOGGER.info("STEP 8 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS 208.67.220.222 response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_CISCO,
					BroadBandTestConstants.STRING_OPEN_DNS_IP_220_222, BroadBandTestConstants.NSLOOKUP_FOR_CISCO);
			if (status) {
				LOGGER.info("STEP 8:ACTUAL :Nslookup validation is successful with DNS 208.67.220.222");
			} else {
				LOGGER.error("STEP 8:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 9:Retrieve ethernet client from connected clients
			 */
			status = false;
			testStep = "s9";
			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 9 : DESCRIPTION :RETRIEVE ETHERNET CLIENT FROM CONNECTED CLIENTS");
			LOGGER.info("STEP 9 : ACTION :ETHERNET CLIENT SHOULD BE RETRIEVED SUCCESSFULLY");
			LOGGER.info("STEP 9 : EXPECTED:ETHERENT RETRIEVED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup response in wifi connected client";
			Dut ethernetClient = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			status = ethernetClient != null;
			if (status) {
				LOGGER.info("STEP 9:ACTUAL :Ethernet connected is retrieved successfully");
			} else {
				LOGGER.error("STEP 9:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
			/**
			 * STEP 10:Execute nslookup facebook.com in ethernet connected client and verify
			 * output
			 */
			status = false;
			testStep = "s10";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 10 : DESCRIPTION :EXECUTE nslookup facebook.com  IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info("STEP 10 : ACTION :EXECUTE COMMAND 'nslookup facebook.com' IN ETHERNET CONNECTED CLIENT");
			LOGGER.info("STEP 10 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK, "",
					BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK);
			if (status) {
				LOGGER.info("STEP 10:ACTUAL :Nslookup validation is successful");
			} else {
				LOGGER.error("STEP 10:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 11:Execute nslookup gmail.com 8.8.8.8 ethernet connected client and
			 * verify output
			 */
			status = false;
			testStep = "s11";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 11 : DESCRIPTION :EXECUTE nslookup gmail.com 8.8.8.8 IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info("STEP 11 : ACTION :EXECUTE COMMAND 'nslookup gmail.com 8.8.8.8' IN ETHERNET CONNECTED CLIENT");
			LOGGER.info("STEP 11 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS 8.8.8.8 response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_GMAIL,
					BroadBandTestConstants.STRING_OPEN_DNS_IP_8, BroadBandTestConstants.NSLOOKUP_FOR_GMAIL);
			if (status) {
				LOGGER.info("STEP 11:ACTUAL :Nslookup validation is successful with DNS 8.8.8.8");
			} else {
				LOGGER.error("STEP 11:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 12:Execute nslookup amazon.com 75.75.76.76 ethernet connected client and
			 * verify output
			 */
			status = false;
			testStep = "s12";
			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 12 : DESCRIPTION :EXECUTE nslookup amazon.com " + openDnsIP
					+ " IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info("STEP 12 : ACTION :EXECUTE COMMAND 'nslookup amazon.com " + openDnsIP
					+ "' IN ETHERNET CONNECTED CLIENT");
			LOGGER.info("STEP 12 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS " + openDnsIP + " response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_AMAZON, openDnsIP,
					BroadBandTestConstants.NSLOOKUP_FOR_AMAZON);
			if (status) {
				LOGGER.info("STEP 12:ACTUAL :Nslookup validation is successful with DNS " + openDnsIP);
			} else {
				LOGGER.error("STEP 12:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 13:Execute nslookup opendns.com 208.67.222.222 ethernet connected client
			 * and verify output
			 */
			status = false;
			testStep = "s13";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 13 : DESCRIPTION :EXECUTE nslookup opendns.com 208.67.222.222 IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info(
					"STEP 13 : ACTION :EXECUTE COMMAND 'nslookup opendns.com 208.67.222.222' IN ETHERNET CONNECTED CLIENT");
			LOGGER.info("STEP 13 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS 208.67.222.222 response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_OPENDNS,
					BroadBandTestConstants.STRING_OPEN_DNS_IP_222, BroadBandTestConstants.NSLOOKUP_FOR_OPENDNS);
			if (status) {
				LOGGER.info("STEP 13:ACTUAL :Nslookup validation is successful with DNS 208.67.222.222");
			} else {
				LOGGER.error("STEP 13:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 14:Execute nslookup yahoo.com 208.67.220.220 ethernet connected client
			 * and verify output
			 */
			status = false;
			testStep = "s14";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 14 : DESCRIPTION :EXECUTE nslookup yahoo.com 208.67.220.220 IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info(
					"STEP 14 : ACTION :EXECUTE COMMAND 'nslookup yahoo.com 208.67.220.220' IN ETHERNET CONNECTED CLIENT");
			LOGGER.info("STEP 14 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS 208.67.220.220 response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_YAHOO,
					BroadBandTestConstants.STRING_OPEN_DNS_IP_220, BroadBandTestConstants.NSLOOKUP_FOR_YAHOO);
			if (status) {
				LOGGER.info("STEP 14:ACTUAL :Nslookup validation is successful with DNS 208.67.220.220");
			} else {
				LOGGER.error("STEP 14:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 15:Execute nslookup comcast.com 208.67.222.220 ethernet connected client
			 * and verify output
			 */
			status = false;
			testStep = "s15";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 15 : DESCRIPTION :EXECUTE nslookup comcast.com 208.67.222.220 IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info(
					"STEP 15 : ACTION :EXECUTE COMMAND 'nslookup comcast.com 208.67.222.220' IN ETHERNET CONNECTED CLIENT");
			LOGGER.info("STEP 15 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS 208.67.222.220 response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_COMCAST,
					BroadBandTestConstants.STRING_OPEN_DNS_IP_222_220, BroadBandTestConstants.NSLOOKUP_FOR_COMCAST);
			if (status) {
				LOGGER.info("STEP 15:ACTUAL :Nslookup validation is successful with DNS 208.67.222.220");
			} else {
				LOGGER.error("STEP 15:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 16:Execute nslookup cisco.com 208.67.220.222 ethernet connected client
			 * and verify output
			 */
			status = false;
			testStep = "s16";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 16 : DESCRIPTION :EXECUTE nslookup cisco.com 208.67.220.222 IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
			LOGGER.info(
					"STEP 16 : ACTION :EXECUTE COMMAND 'nslookup cisco.com 208.67.220.222' IN ETHERNET CONNECTED CLIENT");
			LOGGER.info("STEP 16 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
			errorMessage = "Unable to validate nslookup with DNS 208.67.220.222 response in wifi connected client";
			status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
					ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_CISCO,
					BroadBandTestConstants.STRING_OPEN_DNS_IP_220_222, BroadBandTestConstants.NSLOOKUP_FOR_CISCO);
			if (status) {
				LOGGER.info("STEP 16:ACTUAL :Nslookup validation is successful with DNS 208.67.220.222");
			} else {
				LOGGER.error("STEP 16:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception in Validating Alternate DNS \n" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Disconnect Wifi Radio 2.4Ghz/5Ghz SSID from the device");
			LOGGER.info("POST-CONDITION 1 : ACTION :Disconnect wifi radio 2.4Ghz/5Ghz SSID ");
			LOGGER.info(
					"POST-CONDITION 1 : EXPECTED : Wifi radio 2.4Ghz/5Ghz SSID should be disconnected successfully");
			LOGGER.info("#######################################################################################");
			try {
				BroadBandResultObject bandResultObject = BroadBandConnectedClientUtils
						.disconnectCnnClientFromSsid(tapEnv, device, wifiConnectedClient);
				LOGGER.info("POST CONDITION 1:ACTUAL: WIFI SSID 2.4GHZ/5GHZ Disconnect status: "
						+ bandResultObject.isStatus());
			} catch (Exception exception2) {
				LOGGER.error("EXCEPTION OCCURRED WHILE PERFORMING POST CONFIGURATION 1" + exception2.getMessage());
			}
			LOGGER.info("########################### ENDING POST CONFIGURATION ####################################");

		}
		LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-ALTN-DNS-1001#####################");
	}

	/**
	 * @TestDetails To apply and Verify XDNS ,override with different cases of
	 *              Invalid values of MAC and DNS IPs.
	 * 
	 * 
	 *              STEP 1 : Enable and verify the XDNS feature using webpa param
	 *              Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
	 *              <li>EXPECTED: XDNS feature should be enabled/disabled using
	 *              webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'
	 *
	 *              STEP 2 : Obtain a working Connected client associated with the
	 *              device
	 *              <li>EXPECTED: Connected client should be obtained successfully
	 *
	 *              STEP 3 : Verify if connected client is able to access the
	 *              internet by pinging the site : 'www.google.com'
	 *              <li>EXPECTED: Connected client should be able to access the site
	 *              : 'www.google.com'
	 *
	 *              STEP 4 : Verify the presence of the dnsmasq_servers.conf file in
	 *              the device and Applying xdns override by adding DNS mapping
	 *              table add DNS mapping table with values MacAddress : Wi-Fi
	 *              MacAddress of the connected client , DnsIPv4 : <IPv4> , DnsIPv6
	 *              : <IPv6>
	 *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS
	 *              mapping table should be added successfully.
	 *
	 *              STEP 5 : Verify xdns override at client mac Level by validating
	 *              the domain name of the site : www.seriesw.net on the connected
	 *              client
	 *              <li>EXPECTED: Connected client should be blocked from accessing
	 *              the site : www.seriesw.net
	 *
	 *              STEP 6 : Factory Reset the device and verify if device comes up
	 *              <li>EXPECTED: Device should come up after Factory reset
	 *
	 *              STEP 7 : Set and verify the Global DNS IPv4 value to <IPv4> and
	 *              Global DNS IPv6 value to <IPv6>
	 *              <li>EXPECTED: Global DNS IPv4 and IPv6 values should be set to
	 *              <IPv4> and <IPv6> respectively.
	 * 
	 *              STEP 8 : Enable and verify the XDNS feature using webpa param
	 *              Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
	 *              <li>EXPECTED: XDNS feature should be enabled/disabled using
	 *              webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'
	 * 
	 *              STEP 9 : Verify Captive portal status of the router after
	 *              factory reset and connect the client device to the SSID after
	 *              reactivating the router.
	 *              <li>EXPECTED: Device should be in Captive portal mode after
	 *              factory reset and the connected clients should able to access
	 *              Network after reactivating the router
	 *
	 *              STEP 10 : Verify the presence of the dnsmasq_servers.conf file
	 *              in the device and add DNS mapping table with values MacAddress :
	 *              Wi-Fi MacAddress of the client , DnsIPv4 : <IPv4>, DnsIPv6 :
	 *              <IPv6>
	 *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS
	 *              mapping table should fail to add with the given Invalid values
	 *              of MacAddress/DnsIPv4.
	 *
	 *              STEP 11 : Verify xdns override at client mac Level by validating
	 *              the domain name of the site : www.seriesw.net after applying dns
	 *              override with Invalid macaddress/dnsserver values
	 *              <li>EXPECTED: The response should have 'www.seriesw.net' as the
	 *              domain name for the site : www.seriesw.net
	 *
	 *              STEP 12 : Verify the presence of the dnsmasq_servers.conf file
	 *              in the device and add DNS mapping table with values MacAddress :
	 *              A:B:C:X:Y:Z , DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
	 *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS
	 *              mapping table should fail to add with the given Invalid values
	 *              of MacAddress/DnsIPv4.
	 *
	 *              STEP 13 : Verify xdns override at client mac Level by validating
	 *              the domain name of the site : www.seriesw.net after applying dns
	 *              override with Invalid macaddress/dnsserver values
	 *              <li>EXPECTED: The response should have <site> as the domain name
	 *              for the site : www.seriesw.net
	 *
	 *              STEP 14 : Verify the presence of the dnsmasq_servers.conf file
	 *              in the device and add DNS mapping table with values MacAddress :
	 *              A:B:C:X:Y:Z , DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
	 *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS
	 *              mapping table should fail to add with the given Invalid values
	 *              of MacAddress/DnsIPv4.
	 *
	 *              STEP 15 : Verify xdns override at client mac Level by validating
	 *              the domain name of the site : www.seriesw.net after applying dns
	 *              override with Invalid macaddress/dnsserver values
	 *              <li>EXPECTED: The response should have 'www.seriesw.net' as the
	 *              domain name for the site : www.seriesw.net
	 *
	 *              STEP 16 : Verify the presence of the dnsmasq_servers.conf file
	 *              in the device and add DNS mapping table with values MacAddress :
	 *              A:B:C:X:Y:Z , DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
	 *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS
	 *              mapping table should fail to add with the given Invalid values
	 *              of MacAddress/DnsIPv4.
	 *
	 *              STEP 17 : Verify xdns override at client mac Level by validating
	 *              the domain name of the site : www.seriesw.net after applying dns
	 *              override with Invalid macaddress/dnsserver values
	 *              <li>EXPECTED: The response should have 'www.seriesw.net' as the
	 *              domain name for the site : www.seriesw.net
	 *
	 *              STEP 18 : Verify the presence of the dnsmasq_servers.conf file
	 *              in the device and add DNS mapping table with values MacAddress :
	 *              A:B:C:X:Y:Z , DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
	 *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS
	 *              mapping table should fail to add with the given Invalid values
	 *              of MacAddress/DnsIPv4.
	 *
	 *              STEP 19 : Verify xdns override at client mac Level by validating
	 *              the domain name of the site : www.seriesw.net after applying dns
	 *              override with Invalid macaddress/dnsserver values
	 *              <li>EXPECTED: The response should have <site> as the domain name
	 *              for the site : www.seriesw.net
	 *
	 *              STEP 20 : Verify the presence of the dnsmasq_servers.conf file
	 *              in the device and add DNS mapping table with values MacAddress :
	 *              A:B:C:X:Y:Z, DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
	 *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS
	 *              mapping table should fail to add with the given Invalid values
	 *              of MacAddress/DnsIPv4.
	 *
	 *              STEP 21 : Verify xdns override at client mac Level by validating
	 *              the domain name of the site : www.seriesw.net after applying dns
	 *              override with Invalid macaddress/dnsserver values
	 *              <li>EXPECTED: The response should have <site> as the domain name
	 *              for the site : www.seriesw.net
	 *
	 *              STEP 22 : Verify the presence of the dnsmasq_servers.conf file
	 *              in the device and add DNS mapping table with values MacAddress :
	 *              A:B:C:X:Y:Z , DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
	 *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS
	 *              mapping table should fail to add with the given Invalid values
	 *              of MacAddress/DnsIPv4.
	 *
	 *              STEP 23 : Verify xdns override at client mac Level by validating
	 *              the domain name of the site : www.seriesw.net after applying dns
	 *              override with Invalid macaddress/dnsserver values
	 *              <li>EXPECTED: The response should have 'www.seriesw.net' as the
	 *              domain name for the site : www.seriesw.net
	 *
	 *              STEP 24 : Verify the presence of the dnsmasq_servers.conf file
	 *              in the device and add DNS mapping table with values MacAddress :
	 *              A:B:C:X:Y:Z , DnsIPv4 : Wi-Fi MacAddress of the client, DnsIPv6
	 *              : <IPv6>
	 *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS
	 *              mapping table should fail to add with the given Invalid values
	 *              of MacAddress/DnsIPv4.
	 *
	 *              STEP 25 : Verify xdns override at client mac Level by validating
	 *              the domain name of the site : www.seriesw.net after applying dns
	 *              override with Invalid macaddress/dnsserver values
	 *              <li>EXPECTED: The response should have <site> as the domain name
	 *              for the site : www.seriesw.net
	 * 
	 * 
	 * 
	 * @author Susheela C
	 * @refactor yamini.s
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-XDNS-1004")
	public void applyAndVerifyInvalidValuesForMACAndDnsIPAreNotAcceptedForDnsoverride(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-XDNS-1004");
		LOGGER.info(
				"TEST DESCRIPTION: To apply and Verify XDNS override with different cases of Invalid values of MAC and DNS IPs");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"STEP 1 : Enable and verify the XDNS feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
		LOGGER.info("STEP 2 : Obtain a working Connected client associated with the device");
		LOGGER.info(
				"STEP 3 : Verify if connected client is able to access the internet by pinging the site : 'www.google.com'");

		LOGGER.info(
				"STEP 4 : Verify the presence of the dnsmasq_servers.conf file in the device and Applying xdns override by adding DNS mapping table add DNS mapping table with values MacAddress : Wi-Fi MacAddress of the connected client");
		LOGGER.info(
				"STEP 5 : Verify xdns override at client mac Level by Accessing site : www.seriesw.net on the connected client");
		LOGGER.info("STEP 6 : Factory Reset the device and verify if device comes up");
		LOGGER.info(
				"STEP 7 : Set and verify the Global DNS IPv4 value to '<IPv4>' and Global DNS IPv6 value to '<IPv6>'");
		LOGGER.info(
				"STEP 8 : Enable and verify the XDNS feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");

		LOGGER.info(
				"STEP 9 : Verify Captive portal status of the router after factory reset and connect the client device to the SSID after reactivating the router.");

		LOGGER.info(
				"STEP 10 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table");
		LOGGER.info(
				"STEP 11 : Verify xdns override at client mac Level by validating the domain name of the site : <site> after applying dns override with Invalid macaddress/dnsserver values");
		LOGGER.info(
				"STEP 12 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table ");
		LOGGER.info(
				"STEP 13 : Verify xdns override at client mac Level by validating the domain name of the site : <site> after applying dns override with Invalid macaddress/dnsserver values");
		LOGGER.info(
				"STEP 14 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table");
		LOGGER.info(
				"STEP 15 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
		LOGGER.info(
				"STEP 16 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table ");
		LOGGER.info(
				"STEP 17 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
		LOGGER.info(
				"STEP 18 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table");
		LOGGER.info(
				"STEP 19 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
		LOGGER.info(
				"STEP 20 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table");
		LOGGER.info(
				"STEP 21 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
		LOGGER.info(
				"STEP 22 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table ");
		LOGGER.info(
				"STEP 23 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
		LOGGER.info(
				"STEP 24 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table");
		LOGGER.info(
				"STEP 25 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
		LOGGER.info("#######################################################################################");
		// Holds the test case ID
		String testCaseId = "TC-RDKB-XDNS-104";
		// boolean variable to store the status
		boolean status = false;
		// Test step number
		String stepNumber = null;
		// Error message
		String errorMessage = null;
		try {
			/**
			 * PRE-CONDITIONS
			 */
			executePreConditions(device, testCaseId, true);

			/**
			 * "s1"
			 */
			// Test step number
			stepNumber = "s1";
			// boolean variable to store the status
			status = false;
			setAndVerifyXdnsFeature(device, testCaseId, stepNumber, true);

			/**
			 * "s2", "s3"
			 */
			// Test step number
			stepNumber = "s2";
			// boolean variable to store the status
			status = false;
			Dut connectedClientSettop = verifyConnectedClientAndInternetAccess(device, testCaseId,
					new String[] { "s2", "s3" });

			/**
			 * "s4"
			 */
			// Test step number
			stepNumber = "s4";
			// boolean variable to store the status
			status = false;
			// Mac address of the connected client
			String clientMacAddress = ((Device) connectedClientSettop).getConnectedDeviceInfo().getWifiMacAddress();
			applyDnsoverrideByAddingDNSMappingTable(device,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					clientMacAddress, testCaseId, "s4");
			/**
			 * "s5"
			 */
			// Test step number
			stepNumber = "s5";
			// boolean variable to store the status
			status = false;
			LOGGER.info("Waiting for 10 Seconds...");
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			// string to store dns block site address for level one site from
			// properties file
			String dnsBlockAddressForLevelOneSite = BroadbandPropertyFileHandler
					.getPropertyKeyForlevelOneBlockAddress();
			// string to store site address for level one site from properties
			// file
			String addressForLevelOneSite = BroadbandPropertyFileHandler.getPropertyKeyForlevelOneSiteHostAddress();
			// Connectivity check for Level One site
			verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelOneSite,
					dnsBlockAddressForLevelOneSite, testCaseId, "s5", false);

			/**
			 * "s6"
			 */
			// Test step number
			stepNumber = "s6";
			// boolean variable to store the status
			status = false;
			executeFactoryResetOnTheDevice(device, testCaseId, stepNumber);
			/**
			 * "s7"
			 */
			// Test step number
			stepNumber = "s7";
			// boolean variable to store the status
			status = false;
			setAndVerifyValuesToGblDNSIPv4IPv6(device, testCaseId, stepNumber);
			/**
			 * "s8"
			 */
			// Test step number
			stepNumber = "s8";
			// boolean variable to store the status
			status = false;
			setAndVerifyXdnsFeature(device, testCaseId, stepNumber, true);

			/**
			 * "s9"
			 */
			// Test step number
			stepNumber = "s9";
			// boolean variable to store the status
			status = false;
			verifyCaptivePrtlModeToReactivateRtrAndConnctClient(device, connectedClientSettop, testCaseId, stepNumber);

			/**
			 * "s10", "s11"
			 */
			applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
					addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IPV4_NULL_VALUE,
					BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					testCaseId, new String[] { "s10", "s11" });

			/**
			 * "s12", "s13"
			 */
			applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
					addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IP_WITH_SUBNET_MASK,
					BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					testCaseId, new String[] { "s12", "s13" });

			/**
			 * "s14", "s15"
			 */
			applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
					addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IP_WITH_NETWORK_ADDRESS,
					BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					testCaseId, new String[] { "s14", "s15" });

			/**
			 * "s16", "s17"
			 */
			applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
					addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IP_WITH_BROADCAST_ADDRESS,
					BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					testCaseId, new String[] { "s16", "s17" });

			/**
			 * "s18", "s19"
			 */
			applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
					addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IP_WITH_NULL_BROADCAST_ADDRESS,
					BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					testCaseId, new String[] { "s18", "s19" });

			/**
			 * "s20", "s21"
			 */
			applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
					addressForLevelOneSite,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					BroadBandTestConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_TWO_PRIMARY,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					testCaseId, new String[] { "s20", "s21" });

			/**
			 * "s22", "s23"
			 */
			applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
					addressForLevelOneSite,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					testCaseId, new String[] { "s22", "s23" });

			/**
			 * "s24", "s25"
			 */
			applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
					addressForLevelOneSite, clientMacAddress,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					testCaseId, new String[] { "s24", "s25" });

		} catch (TestException exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("TC-RDKB-XDNS-1004 : Execution error occured due to exception --> " + errorMessage);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
		} finally {
			BroadBandPostConditionUtils.executePostConditionForXdns(device, tapEnv, true);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
	}

	/**
	 * Common step for executing pre-conditions. ie., to set the Global DNS IPV4
	 * value to "<IPv4>" and Global DNS IPV6 value to "<IPv6>" and Verify and enable
	 * 2.4GHz and 5GHz private wifi connection.
	 * 
	 * @param device
	 * @param testCaseId
	 * 
	 * @refactor yamini.s
	 */
	private void executePreConditions(Dut device, String testCaseId, boolean enablePrivateSsidandReactiveRouter)
			throws TestException {
		// boolean variable to store the status
		boolean status = false;
		// Error message
		String errorMessage = null;

		LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"PRE-CONDITION-1: DESCRIPTION : Set and verify the Global DNS IPv4 value to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
		LOGGER.info(
				"PRE-CONDITION-1: ACTION : Set the Global DNS IPv4 value to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
		LOGGER.info(
				"PRE-CONDITION-1: EXPECTED : Global DNS IPv4 value should be set to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
		LOGGER.info("#######################################################################################");
		status = false;
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, WebPaDataTypes.STRING.getValue(),
				AutomaticsPropertyUtility
						.getProperty(BroadBandPropertyKeyConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE),
				BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		// Error message
		errorMessage = "Failed to set Global DNS IPv4 value to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'";
		LOGGER.info("PRE-CONDITION-1: ACTUAL: "
				+ (status ? "Global DNS IPv4 value sucessfully set to '<IPv4>'" : errorMessage));

		if (!status) {
			throw new TestException(
					BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-1 FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"PRE-CONDITION-2: DESCRIPTION : Set and verify the Global DNS IPv6 value to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
		LOGGER.info(
				"PRE-CONDITION-2: ACTION : Set the Global DNS IPv6 value to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
		LOGGER.info(
				"PRE-CONDITION-2: EXPECTED : Global DNS IPv6 value should be set to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
		LOGGER.info("#######################################################################################");
		status = false;
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6, WebPaDataTypes.STRING.getValue(),
				BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV6_VALUE, BroadBandTestConstants.THREE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		// Error message
		errorMessage = "Failed to set Global DNS IPv6 value to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'";
		LOGGER.info("PRE-CONDITION-2: ACTUAL: "
				+ (status ? "Global DNS IPv6 value sucessfully set to '<IPv6>'" : errorMessage));
		if (!status) {
			throw new TestException(
					BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-2 FAILED : " + errorMessage);
		}
		if (enablePrivateSsidandReactiveRouter) {
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION-3: DESCRIPTION : Verify 2.4GHz private SSID status on the Device and Enable using webpa param 'Device.WiFi.SSID.10001.Enable'");
			LOGGER.info(
					"PRE-CONDITION-3: ACTION : Get 2.4GHz private SSID status on the Device using webpa param 'Device.WiFi.SSID.10001.Enable'");
			LOGGER.info(
					"PRE-CONDITION-3: EXPECTED : 2.4GHz private SSID should be set to enabled using webpa param 'Device.WiFi.SSID.10001.Enable'");
			LOGGER.info("#######################################################################################");
			status = false;
			String successMessage = "The 2.4GHz private SSID on the Device is already enabled.";
			String response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE);
			status = CommonMethods.isNotNull(response)
					&& CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.TRUE);
			// Error message
			errorMessage = "Unable get the 2.4GHz private SSID status on this Device.";
			if (!status) {
				// Error message
				errorMessage = "Unable to Enable the 2.4GHz private SSID on this Device.";
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE,
						WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE,
						BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				successMessage = "The 2.4GHz private SSID on the Device enabled successfully using webpa param 'Device.WiFi.SSID.10001.Enable'.";
			}

			LOGGER.info("PRE-CONDITION-3: ACTUAL: " + (status ? successMessage : errorMessage));
			if (!status) {
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-3 FAILED : " + errorMessage);
			}

			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION-4: DESCRIPTION : Verify 5GHz private SSID status on the Device and Enable using webpa param 'Device.WiFi.SSID.10101.Enable'");
			LOGGER.info(
					"PRE-CONDITION-4: ACTION : Get 5GHz private SSID status on the Device using webpa param 'Device.WiFi.SSID.10101.Enable'");
			LOGGER.info(
					"PRE-CONDITION-4: EXPECTED : 5GHz private SSID should be set to enabled using webpa param 'Device.WiFi.SSID.10101.Enable'");
			LOGGER.info("#######################################################################################");
			status = false;
			successMessage = "The 5GHz private SSID on the Device is already enabled.";
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS);
			status = CommonMethods.isNotNull(response)
					&& CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.TRUE);
			// Error message
			errorMessage = "Unable get the 5GHz private SSID status on this Device.";
			if (!status) {
				// Error message
				errorMessage = "Unable to Enable the 5GHz private SSID on this Device.";
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE,
						BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				successMessage = "The 5GHz private SSID on the Device enabled successfully using webpa param 'Device.WiFi.SSID.10101.Enable'.";
			}
			LOGGER.info("PRE-CONDITION-4: ACTUAL: " + (status ? successMessage : errorMessage));
			if (!status) {
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-4 FAILED : " + errorMessage);
			}

			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION-5: DESCRIPTION : Re-Activate and verify the device by setting 2.4GHz and 5GHz SSID and Passphrase");
			LOGGER.info(
					"PRE-CONDITION-5: ACTION : Re-Activate the device by setting 2.4GHz and 5GHz SSID and Passphrase");
			LOGGER.info("PRE-CONDITION-5: EXPECTED : Device should be Re-Activated successfully");
			LOGGER.info("#######################################################################################");
			status = false;
			try {
				BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
				status = true;
			} catch (TestException e) {
				errorMessage = e.getMessage();
			}
			LOGGER.info(
					"PRE-CONDITION-5: ACTUAL: " + (status ? "Router Device Re-Activated successfully." : errorMessage));
			if (!status) {
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-5 FAILED : " + errorMessage);
			}
		}
		LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	}

	/**
	 * Common step to set and verify the Xdns Feature using webpa param
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
	 * 
	 * @param dut
	 * @param testCaseId
	 * @param stepNumber
	 * @param valueToSet boolean value to enable or disable the xdns feature
	 * 
	 * @refactor yamini.s
	 */

	private void setAndVerifyXdnsFeature(Dut device, String testCaseId, String stepNumber, boolean valueToSet) {
		/**
		 * STEP: Enable/Disable and verify the XDNS feature using webpa param
		 * Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
		 * <li>EXPECTED: XDNS feature should be enabled/disabled using webpa param
		 * 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'
		 */
		// boolean variable to store the status
		boolean status = false;
		// XDNS Value
		String xdnsValue = valueToSet ? BroadBandTestConstants.TRUE : BroadBandTestConstants.FALSE;
		// Error message
		String errorMessage = valueToSet
				? "Failed to enable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
				: "Failed to disable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
		// String to store the step description
		String stepDescription = valueToSet
				? "Enable and verify the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
				: "Disable and verify the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
		// String to store the expected result
		String expectedResult = valueToSet
				? "XDNS feature should be enabled using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
				: "XDNS feature should be disabled using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
		// String to store the success message
		String successMessage = valueToSet
				? "XDNS feature enabled sucessfully using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
				: "XDNS feature disabled sucessfully using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
		// String to store the action
		String action = valueToSet
				? "Enable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
				: "Disable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";

		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : " + stepDescription);
		LOGGER.info("STEP :  " + stepNumber + " : ACTION : " + action);
		LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: " + expectedResult);
		LOGGER.info("#######################################################################################");
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.CONSTANT_3,
				xdnsValue, BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		LOGGER.info("STEP " + stepNumber + " - ACTUAL: " + (status ? successMessage : errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, valueToSet);
	}

	/**
	 * Common step to get a 2.4GHz/5GHz connected client device associated with the
	 * device
	 * 
	 * @param device
	 * @param testCaseId
	 * @param stepNumbers
	 * 
	 * @return connectedClientSettop
	 * 
	 * @refactor yamini.s
	 */

	private Dut verifyConnectedClientAndInternetAccess(Dut device, String testCaseId, String[] stepNumbers) {
		// Connected client instance
		Dut connectedClientSettop = null;
		if (stepNumbers.length == 2) {
			/**
			 * STEP: Obtain a working Connected client associated with the device
			 * <li>EXPECTED: Connected client should be obtained successfully
			 */
			// boolean variable to store the status
			boolean status = false;
			// Test step number
			String stepNumber = stepNumbers[0];
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : Obtain a working Connected client associated with the device");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : Obtain a working Connected client");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: Connected client should be obtained successfully");
			LOGGER.info("#######################################################################################");
			// Error message
			String errorMessage = "No working connect client is associated with the device";
			try {
				connectedClientSettop = BroadBandConnectedClientUtils
						.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
				status = (connectedClientSettop != null);
			} catch (TestException exeception) {
				// Error message
				errorMessage = exeception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (connectedClientSettop != null);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ":ACTUAL: Connected client associated with the device obtained successfully.");
			} else {
				LOGGER.error("STEP " + stepNumber + ":ACTUAL: " + errorMessage);

			}
			LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
					+ (status ? "Connected client associated with the device obtained successfully." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			tapEnv.waitTill(BroadBandTestConstants.FOUR_MINUTES);

			/**
			 * STEP : Verify if connected client is able to access the internet by pinging
			 * the site : 'www.google.com'
			 * <li>EXPECTED: Connected client should be able to access the site :
			 * 'www.google.com'
			 */
			// boolean variable to store the status
			status = false;
			// Test step number
			stepNumber = stepNumbers[1];
			// Error message
			errorMessage = "Connected client not able to access the site : 'www.google.com'";
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify if connected client is able to access the internet by pinging the site : 'www.google.com'");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute command WINDOWS : ping www.google.com -n 1 | grep 'Reply from' or LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' on the connected client");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Connected client should be able to access the site : 'www.google.com'");
			LOGGER.info("#######################################################################################");
			status = ConnectedNattedClientsUtils.verifyNetworkConnection(connectedClientSettop, tapEnv, false);
			LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
					+ (status ? "Connected client able to access the site : 'www.google.com' successfully."
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		}
		return connectedClientSettop;
	}

	/**
	 * Common step to apply and verify Dnsoverride by adding DNS mapping table
	 * 
	 * @param device
	 * @param xdnsOverRideLevelIpv4
	 * @param xdnsOverRideLevelIpv6
	 * @param clientMacAddress
	 * @param testCaseId
	 * @param stepNumber
	 * 
	 * @refactor yamini.s
	 */

	private void applyDnsoverrideByAddingDNSMappingTable(Dut device, String xdnsOverRideLevelIpv4,
			String xdnsOverRideLevelIpv6, String clientMacAddress, String testCaseId, String stepNumber) {

		/**
		 * STEP : Verify the presence of the dnsmasq_servers.conf file in the device and
		 * Add DNS mapping table
		 * 
		 * <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table
		 * should be added successfully.
		 */
		// boolean variable to store the status
		boolean status = false;
		// Error message
		String errorMessage = "Attempt to apply dnsoverride through DNS mapping table failed.";
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : "
				+ clientMacAddress + ", DnsIPv4 : " + xdnsOverRideLevelIpv4 + ", DnsIPv6 : " + xdnsOverRideLevelIpv6
				+ ".");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : Add DNS mapping table using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DNSMappingTable.'");
		LOGGER.info(
				"EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should be added successfully.");
		LOGGER.info("#######################################################################################");
		status = BroadBandConnectedClientUtils.applyXdnsOverRideAddingDNSMapTbl(tapEnv, device, clientMacAddress,
				xdnsOverRideLevelIpv4, xdnsOverRideLevelIpv6);
		LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
				+ (status ? "Applied dnsoverride by adding DNS mapping table successfully." : errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	}

	/**
	 * Common step to verify network connectivity in the connected client by
	 * accessing "www.google.com" after blocking/allowing the connected client
	 * through DNS mapping table.
	 * 
	 * @param device
	 * @param connectedClientSettop
	 * @param siteAddressToPing
	 * @param testCaseId
	 * @param stepNumber
	 * @param allowSiteAccess       boolean variable representing whether the
	 *                              connected client should be blocked or allowed
	 *                              from accessing the site
	 * 
	 * @refactor yamini.s
	 */

	private void verifySiteAccessInConnectClient(Dut device, Dut connectedClientSettop, String siteAddressToPing,
			String expectedSiteAddress, String testCaseId, String stepNumber, boolean allowSiteAccess) {

		/**
		 * STEP : Verify xdns override at client mac Level by validating the domain name
		 * of the site passed after blocking/allowing the connected client through DNS
		 * mapping table.
		 * <li>EXPECTED: The corresponding site should be allowed/blocked as expected.
		 */

		// boolean variable to store the status
		boolean status = false;
		// Error message
		String errorMessage = allowSiteAccess
				? "The response doesn't have '" + expectedSiteAddress + "' as domain name for the site : '"
						+ siteAddressToPing
				: "The response doesn't have '" + expectedSiteAddress + "' as domain name for the site : '"
						+ siteAddressToPing + "', Even after blocking the site through DNS table.";
		// String to store the step description
		String stepDescription = allowSiteAccess
				? "Verify xdns override at client mac Level by validating the domain name of the site : "
						+ siteAddressToPing + ", After Factory resetting the router"
				: "Verify xdns override at client mac Level by validating the domain name of the site : "
						+ siteAddressToPing + ", After applying DNS override at client mac level";
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : " + stepDescription);
		LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command 'nslookup " + siteAddressToPing
				+ " <IPv4>' on the connected client");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : The response should have '" + expectedSiteAddress
				+ "' as domain name for the site : '" + siteAddressToPing + "'");
		LOGGER.info("#######################################################################################");
		long startTime = System.currentTimeMillis();
		// Polling for 120 sec
		do {
			status = BroadBandConnectedClientUtils
					.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv, connectedClientSettop,
							siteAddressToPing,
							AutomaticsPropertyUtility
									.getProperty(BroadBandPropertyKeyConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE),
							expectedSiteAddress);
			if (status) {
				break;
			}
			// Waiting for 30 sec
			LOGGER.info("Going to wait for 30sec and retry.");
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		} while (System.currentTimeMillis() < (startTime + BroadBandTestConstants.TWO_MINUTES) && (!status));

		LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
				+ (status
						? "The response has '" + expectedSiteAddress + "' as domain name for the site : '"
								+ siteAddressToPing + "' as expected."
						: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	}

	/**
	 * Common step to execute Factory reset on the router device
	 * 
	 * @param device     The device on which the factory reset has to be performed
	 * @param testCaseId
	 * 
	 * @param stepNumber
	 * 
	 * @refactor yamini.s
	 */

	private void executeFactoryResetOnTheDevice(Dut device, String testCaseId, String stepNumber) {
		/**
		 * STEP : Factory Reset the device and verify if device comes up
		 * <li>EXPECTED: Device should come up after Factory reset
		 */
		// boolean variable to store the status
		boolean status = false;
		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"STEP :  " + stepNumber + " : DESCRIPTION : Factory Reset the device and verify if device comes up");
		LOGGER.info("STEP :  " + stepNumber
				+ " : ACTION : Factory Reset the device using webpa param Device.X_CISCO_COM_DeviceControl.FactoryReset");
		LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: Device should come up after Factory reset");
		LOGGER.info("#######################################################################################");
		// Error message
		String errorMessage = "Failed to Tigger Factory Reset/Device didn't come up after Factory Reset.";
		status = BroadBandCommonUtils.performFactoryResetWebPaByPassingTriggerTime(tapEnv, device,
				BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS);
		if (status) {
			LOGGER.info("STEP :  " + stepNumber + " ACTUAL : Device came up after Factory reset successfully.");
		} else {
			LOGGER.error("STEP :  " + stepNumber + " ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	}

	/**
	 * Common step to set default values to Global DNS IPv4 and Global DNS IPv6
	 * 
	 * @param device
	 * @param testCaseId
	 * @param stepNumber
	 * 
	 * @refactor yamini.s
	 */

	private void setAndVerifyValuesToGblDNSIPv4IPv6(Dut device, String testCaseId, String stepNumber) {
		/**
		 * STEP : Set and verify the Global DNS IPv4 value to '<IPv4>' and Global DNS
		 * IPv6 value to '<IPv6>'
		 * <li>EXPECTED: Global DNS IPv4 and IPv6 values should be set to '<IPv4>' and
		 * '<IPv6>' respectively.
		 */
		// boolean variable to store the status
		boolean status = false;
		// Error message
		String errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : Set and verify the Global DNS IPv4 value to '<IPv4>' and Global DNS IPv6 value to '<IPv6>'");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION 1 : Set the Global DNS IPv4 value to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION 2 : Set the Global DNS IPv6 value to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
		LOGGER.info("STEP " + stepNumber
				+ ": EXPECTED : Global DNS IPv4 and IPv6 values should be set to '<IPv4>' and '<IPv6>' respectively.");
		LOGGER.info("#######################################################################################");
		// Error message
		errorMessage = "Failed to set Global DNS IPv4 value to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'";
		if (BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, WebPaDataTypes.STRING.getValue(),
				AutomaticsPropertyUtility
						.getProperty(BroadBandPropertyKeyConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE),
				BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {
			// Error message
			errorMessage = "Failed to set Global DNS IPv6 value to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6, WebPaDataTypes.STRING.getValue(),
					BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV6_VALUE, BroadBandTestConstants.THREE_MINUTES,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		}
		LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
				+ (status ? "Global DNS IPv4 and IPv6 values sucessfully set to '<IPv4>' and '<IPv6>' respectively."
						: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	}

	/**
	 * Common step to verify that the router device is in CaptivePortal Mode after
	 * factory reset, reactivate and connect the client device to the SSID.
	 * 
	 * @param device
	 * @param connectedClientDevice
	 * @param testCaseId
	 * @param stepNumber
	 * 
	 * @refactor yamini.s
	 */

	private void verifyCaptivePrtlModeToReactivateRtrAndConnctClient(Dut device, Dut connectedClientSettop,
			String testCaseId, String stepNumber) {
		/**
		 * STEP : Verify Captive portal status of the router after factory reset and
		 * connect the client device to the SSID after reactivating the router.
		 * <li>EXPECTED: Device should be in Captive portal mode after factory reset and
		 * the connected clients should able to access Network after reactivating the
		 * router
		 */
		// boolean variable to store the status
		boolean status = false;
		String ssidName = null;
		// Error message
		String errorMessage = null;
		String passPhraseName = null;

		LOGGER.info("#######################################################################################");

		if (!DeviceModeHandler.isDSLDevice(device)) {
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Captive portal status of the router after factory reset and connect the client device to the SSID after reactivating the router.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : Get Captive portal status using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable and re-activate the router.");
			LOGGER.info(
					"EXPECTED: Device should be in Captive portal mode after factory reset and the client device should be able to connect to the SSID after reactivating the router.");
		} else {
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : After factory reset connect the client device to the SSID after reactivating the router.");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : Re-activate the router.");
			LOGGER.info(
					"EXPECTED: After factory reset the client device should be able to connect to the SSID after reactivating the router.");
		}
		LOGGER.info("#######################################################################################");

		boolean captivePortalStatus = true;
		try {
			if (!DeviceModeHandler.isDSLDevice(device)) {
				// Error message
				errorMessage = "Device is not in Captive portal after factory resetting.";
				captivePortalStatus = BroadBandWiFiUtils.verifyCaptivePortalModeUsingWebPaCommand(tapEnv, device);
			}
			if (captivePortalStatus) {
				if (!DeviceModeHandler.isDSLDevice(device)) {
					LOGGER.info("Captive portal is enabled in the router after factory reset.");
				}
				LOGGER.info("Going to Reactivate the router.");
				try {
					// 'reactivateDeviceUsingWebPa' method throws TestException if
					// the re-activation process fails.
					BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
					LOGGER.info("Successfully Reactivated the router.");
					String wifiCapability = ((Device) connectedClientSettop).getConnectedDeviceInfo()
							.getWifiCapability();
					ssidName = (CommonMethods.isNotNull(wifiCapability) && (wifiCapability
							.equalsIgnoreCase(BroadBandConnectedClientTestConstants.STRING_WIFI_CAPABILITY_2_4GHZ_ONLY)
							|| wifiCapability.equalsIgnoreCase(
									BroadBandConnectedClientTestConstants.STRING_WIFI_CAPABILITY_DUAL_BAND)))
											? BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(
													device, tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ)
											: BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(
													device, tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);

					passPhraseName = (CommonMethods.isNotNull(wifiCapability) && (wifiCapability
							.equalsIgnoreCase(BroadBandConnectedClientTestConstants.STRING_WIFI_CAPABILITY_2_4GHZ_ONLY)
							|| wifiCapability.equalsIgnoreCase(
									BroadBandConnectedClientTestConstants.STRING_WIFI_CAPABILITY_DUAL_BAND)))
											? BroadBandConnectedClientUtils
													.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device, tapEnv,
															WiFiFrequencyBand.WIFI_BAND_2_GHZ)
											: BroadBandConnectedClientUtils
													.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device, tapEnv,
															WiFiFrequencyBand.WIFI_BAND_5_GHZ);
					// Error message
					errorMessage = "Unable to obtain SSID/passphrase from the router";
					if (CommonMethods.isNotNull(ssidName) && CommonMethods.isNotNull(passPhraseName)) {
						LOGGER.info("Obtained SSID : " + ssidName + " and PassPhrase : " + passPhraseName
								+ ". Going to connect the client with the router.");
						status = ConnectedNattedClientsUtils.connectToSSID(connectedClientSettop, tapEnv, ssidName,
								passPhraseName);
						errorMessage = "Unable to connect the client to the router after reactivating/Factory Resetting the router.";
					}
				} catch (TestException e) {
					errorMessage = e.getMessage();
				}
			}
		} catch (Exception exe) {
			errorMessage = "Following Exception occurred while querying the Captive portal status using WebPA/Dmcli command  -> "
					+ exe.getMessage();
		}
		if (!DeviceModeHandler.isDSLDevice(device)) {
			LOGGER.info("STEP " + stepNumber + " - ACTUAL: " + (status
					? "Router is in Captive portal mode after factory reset. The client device is connected to the SSID successfully after reactivating the router."
					: errorMessage));
		} else {
			LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
					+ (status ? "The client device is connected to the SSID successfully after reactivating the router."
							: errorMessage));
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	}

	/**
	 * Common step to apply and verify different cases of Invalid values for MAC and
	 * DNS IP are not accepted for dnsoverride
	 * 
	 * @param device
	 * @param connectedClientSettop
	 * @param siteAddressToPing
	 * @param xdnsOverRideLevelIpv4
	 * @param clientMacAddress
	 * @param xdnsOverRideLevelIpv6
	 * @param testCaseId
	 * @param stepNumbers
	 * 
	 * @refactor yamini.s
	 */

	private void applyInvalDnsoverrideValsAndVerifySiteNtBlckd(Dut device, Dut connectedClientSettop,
			String siteAddressToPing, String expectedsiteAddress, String xdnsOverRideLevelIpv4, String clientMacAddress,
			String xdnsOverRideLevelIpv6, String testCaseId, String[] stepNumbers) {

		// Test step number
		String stepNumber = null;
		// boolean variable to store the status
		boolean status = false;
		// Error message
		String errorMessage = null;
		if (stepNumbers.length == 2) {
			/**
			 * STEP : Verify the presence of the dnsmasq_servers.conf file in the device and
			 * add DNS mapping table with given values
			 * 
			 * <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table
			 * should be added successfully with the given Invalid values of
			 * MacAddress/DnsIPv4.
			 */
			// Test step number
			stepNumber = stepNumbers[0];
			// boolean variable to store the status
			status = false;
			errorMessage = "Attempt to apply dnsoverride with invalid macaddress/dnsserver value to DNS mapping table failed.";
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : "
					+ clientMacAddress + ", DnsIPv4 : " + xdnsOverRideLevelIpv4 + ", DnsIPv6 : " + xdnsOverRideLevelIpv6
					+ ".");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Add DNS mapping table using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DNSMappingTable.'");
			LOGGER.info(
					"EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should be added successfully with the given Invalid values of MacAddress/DnsIPv4.");
			LOGGER.info("#######################################################################################");
			status = BroadBandConnectedClientUtils.applyXdnsOverRideAddingDNSMapTbl(tapEnv, device, clientMacAddress,
					xdnsOverRideLevelIpv4, xdnsOverRideLevelIpv6);
			LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
					+ (status ? "Added DNS Mapping table with invalid macaddress/dnsserver value successfully."
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * STEP : Verify xdns override at client mac Level by validating the domain name
			 * of the site passed after applying dns override with Invalid
			 * macaddress/dnsserver values
			 * <li>EXPECTED: The response should have the site name as domain name
			 */
			// Test step number
			stepNumber = stepNumbers[1];
			// boolean variable to store the status
			status = false;
			// Error message
			errorMessage = "The response doesn't have '" + expectedsiteAddress + "' as domain name for the site : '"
					+ siteAddressToPing + "'.";
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify xdns override at client mac Level by validating the domain name of the site : "
					+ siteAddressToPing + ", after applying dns override with Invalid macaddress/dnsserver values");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command 'nslookup " + siteAddressToPing
					+ " <IPv4>' on the connected client");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : The response should have '" + siteAddressToPing
					+ "' as domain name for the site : '" + siteAddressToPing + "'");
			LOGGER.info("#######################################################################################");
			status = BroadBandConnectedClientUtils
					.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv, connectedClientSettop,
							siteAddressToPing,
							AutomaticsPropertyUtility
									.getProperty(BroadBandPropertyKeyConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE),
							expectedsiteAddress);
			LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
					+ (status
							? "The response has '" + siteAddressToPing + "' as domain name for the site : '"
									+ siteAddressToPing + "', as expected."
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		}
	}

	/**
	 * This Test case is written to apply and verify Xdns override at gateway level.
	 * 
	 * STEP 1 : Enable and verify the XDNS feature using webpa param
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
	 * <li>EXPECTED: XDNS feature should be enabled using webpa param
	 * 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'
	 *
	 * STEP 2 : Obtain a working Connected client associated with the device
	 * <li>EXPECTED: Connected client should be obtained successfully
	 *
	 * STEP 3 : Verify if connected client is able to access the internet by pinging
	 * the site : 'www.google.com'
	 * <li>EXPECTED: Connected client should be able to access the site :
	 * 'www.google.com'
	 *
	 * STEP 4 : Set and verify the Global DNS IPv4 value using webpa param
	 * 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'
	 * <li>EXPECTED : Global DNS IPv4 value should be set using webpa param
	 * 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'
	 *
	 * STEP 5 : Verify xdns override at gateway Level by validating the domain name
	 * of the site : www.seriesw.net on the connected client
	 * <li>EXPECTED: DNS request for server should fail with 'timed out' error
	 * 
	 * @param Dut The device to be used.
	 * @author Susheela C
	 * @Refactor Alan_Bivera
	 * 
	 */

	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			TestGroup.XDNS })
	@TestDetails(testUID = "TC-RDKB-XDNS-1001")
	public void applyAndVerifyXdnsOverrideAtGatewayLevel(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-XDNS-1001");
		LOGGER.info("TEST DESCRIPTION: To apply and verify Xdns override at gateway level");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"STEP 1 : Enable and verify the XDNS feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
		LOGGER.info("STEP 2 : Obtain a working Connected client associated with the device");
		LOGGER.info(
				"STEP 3 : Verify if connected client is able to access the internet by pinging the site : 'www.google.com'");
		LOGGER.info(
				"STEP 4 : Set and verify the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
		LOGGER.info(
				"STEP 5 : Verify xdns override at gateway Level by validating the domain name of the site : www.seriesw.net on the connected client");
		LOGGER.info("#######################################################################################");
		// Holds the test case ID
		String testCaseId = "TC-RDKB-XDNS-101";
		// boolean variable to store the status
		boolean status = false;
		// Test step number
		String stepNumber = null;
		// Error message
		String errorMessage = null;
		try {

			executePreConditions(device, testCaseId, true);
			// Test step number
			stepNumber = "s1";
			// boolean variable to store the status
			status = false;
			setAndVerifyXdnsFeature(device, testCaseId, stepNumber, true);

			// Test step number
			stepNumber = "s2";
			// boolean variable to store the status
			status = false;
			Dut connectedClientSettop = verifyConnectedClientAndInternetAccess(device, testCaseId,
					new String[] { "s2", "s3" });

			/**
			 * STEP 4 : Set and verify the Global DNS IPv4 value using webpa param
			 * 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'
			 * <li>EXPECTED : Global DNS IPv4 value should be set using webpa param
			 * 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'
			 */
			// step number
			stepNumber = "s4";
			// boolean variable to store the status
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Set and verify the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : ACTION : Set the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : EXPECTED : Global DNS IPv4 value should be set using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
			LOGGER.info("#######################################################################################");
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, WebPaDataTypes.STRING.getValue(),
					BroadBandTestConstants.STRING_DNS_GATEWAY_LEVEL_OVERRIDE_IPV4_VALUE,
					BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			// Error message
			errorMessage = "Failed to set Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'";
			LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
					+ (status ? "Global DNS IPv4 value sucessfully set" : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * STEP 5 : Verify xdns override at gateway Level by validating the domain name
			 * of the site : www.seriesw.net on the connected client
			 * <li>EXPECTED: DNS request for server should fail with 'timed out' error
			 */
			stepNumber = "s5";
			// boolean variable to store the status
			status = false;
			// string to store site address for level one site from properties
			// file
			String addressForLevelOneSite = AutomaticsTapApi
					.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_HOST_ADDRESS_FOR_LEVEL_ONE_SITE);
			// Error message
			errorMessage = "The response has '" + addressForLevelOneSite + "' as domain name for the site : "
					+ addressForLevelOneSite + "', Even after blocking the client at gateway level.";
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify xdns override at gateway Level by validating the domain name of the site : "
					+ addressForLevelOneSite + ", after blocking the client at gateway level");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command 'nslookup " + addressForLevelOneSite
					+ " on the connected client");
			LOGGER.info(
					"STEP " + stepNumber + ": EXPECTED : DNS request for server should fail with 'timed out' error");
			LOGGER.info("#######################################################################################");
			// Instance to store the client settop
			Device ecastSettop = (Device) connectedClientSettop;
			// String to store ssh command
			String sshCommand = BroadBandCommonUtils.concatStringUsingStringBuffer("nslookup",
					AutomaticsConstants.SPACE, addressForLevelOneSite, AutomaticsConstants.SPACE,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE));
			long startTime = System.currentTimeMillis();
			// Polling for 120 sec
			do {
				// String to store response obtained from connected client
				String response = tapEnv.executeCommandOnOneIPClients(ecastSettop, sshCommand);
				status = (CommonMethods.patternMatcher(response,
						BroadBandTestConstants.STRING_DNS_REQUEST_TIMED_OUT_ERROR_MESSAGE)
						|| !BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
								connectedClientSettop, addressForLevelOneSite,
								AutomaticsPropertyUtility.getProperty(
										BroadBandPropertyKeyConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE),
								addressForLevelOneSite));
				if (status) {
					break;
				}
				// Waiting for 30 sec
				LOGGER.info("Going to wait for 30sec and retry.");
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			} while (System.currentTimeMillis() < (startTime + BroadBandTestConstants.TWO_MINUTES) && (!status));

			LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
					+ (status ? "DNS request for server failed with 'timed out' error as expected." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

		} catch (TestException exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("TC-RDKB-XDNS-1001 : Execution error occured due to exception --> " + errorMessage);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
		} finally {
			BroadBandPostConditionUtils.executePostConditionForXdns(device, tapEnv, false);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			LOGGER.info("TEST CASE END : TC-RDKB-XDNS-1001");
		}
	}

	/**
	 * This Test case is written as a part of automation testing to apply and Verify
	 * XDNS override of Primary Level 1 ,2 and 3 IPv4 and IPv6 dns server IPs for
	 * connected client.
	 * 
	 * STEP 1 : Enable and verify the XDNS feature using webpa param
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
	 * <li>EXPECTED: XDNS feature should be enabled/disabled using webpa param
	 * 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'
	 *
	 * STEP 2 : Obtain a working Connected client associated with the device
	 * <li>EXPECTED: Connected client should be obtained successfully
	 *
	 * STEP 3 : Verify if connected client is able to access the internet by pinging
	 * the site : 'www.google.com'
	 * <li>EXPECTED: Connected client should be able to access the site :
	 * 'www.google.com'
	 *
	 * STEP 4 : Verify the presence of the dnsmasq_servers.conf file in the device
	 * and add DNS mapping table with values MacAddress : Wi-Fi MacAddress of the
	 * connected client
	 * <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table
	 * should be added successfully.
	 *
	 * STEP 5 : Verify xdns override at client mac Level by validating the domain
	 * name of the site : www.seriesw.net on the connected client
	 * <li>EXPECTED: The response should have 'xdns-low-gslb.gslb2.comcast.com' as
	 * domain name for the site : 'www.seriesw.net'
	 *
	 * STEP 6 : Verify the presence of the dnsmasq_servers.conf file in the device
	 * and add DNS mapping table with values MacAddress : Wi-Fi MacAddress of the
	 * connected client , DnsIPv4 : 75.75.75.20 , DnsIPv6 : 2001:558:feed::7520
	 * <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table
	 * should be added successfully.
	 *
	 * STEP 7 : Verify xdns override at client mac Level by validating the domain
	 * name of the site : www.plannedparenthood.org on the connected client
	 * <li>EXPECTED: The response should have 'xdns-medium-gslb.gslb2.comcast.com'
	 * as domain name for the site : 'www.plannedparenthood.org'
	 *
	 * STEP 8 : Verify the presence of the dnsmasq_servers.conf file in the device
	 * and add DNS mapping table with values MacAddress : Wi-Fi MacAddress of the
	 * connected client
	 * <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table
	 * should be added successfully.
	 *
	 * STEP 9 : Verify xdns override at client mac Level by validating the domain
	 * name of the site : www.ar15.com on the connected client
	 * <li>EXPECTED: The response should have 'xdns-high-gslb.gslb2.comcast.com' as
	 * domain name for the site : 'www.ar15.com'
	 *
	 * STEP 10 : Factory Reset the device and verify if device comes up
	 * <li>EXPECTED: Device should come up after Factory reset
	 *
	 * STEP 11 : Set and verify the Global DNS IPv4 value and Global DNS IPv6 value
	 * <li>EXPECTED: Global DNS IPv4 and IPv6 values should be set.
	 * 
	 * STEP 12 : Verify Captive portal status of the router after factory reset and
	 * connect the client settop to the SSID after reactivating the router.
	 * <li>EXPECTED: Device should be in Captive portal mode after factory reset and
	 * the connected clients should able to access Network after reactivating the
	 * router
	 *
	 * STEP 13 : Verify connected client is able to access the site :
	 * www.seriesw.net, After Factory resetting the router
	 * <li>EXPECTED: The response should have 'www.seriesw.net' as the domain name
	 * for the site : www.seriesw.net
	 *
	 * STEP 14 : Verify connected client is able to access the site :
	 * www.plannedparenthood.org, After Factory resetting the router
	 * <li>EXPECTED: The response should have 'www.plannedparenthood.org' as the
	 * domain name for the site : www.plannedparenthood.org
	 *
	 * STEP 15 : Verify connected client is able to access the site : www.ar15.com,
	 * After Factory resetting the router
	 * <li>EXPECTED: The response should have 'www.ar15.com' as the domain name for
	 * the site : www.ar15.com
	 * 
	 * @param device The Dut to be used.
	 * @author Susheela C
	 * @refactor Alan_Bivera
	 */

	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.XDNS })
	@TestDetails(testUID = "TC-RDKB-XDNS-1002")
	public void applyAndVerifyXdnsOverrideAtClientMacLevelPrimaryUsingIPv4AndIPv6(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-XDNS-1002");
		LOGGER.info(
				"TEST DESCRIPTION: To apply and Verify XDNS override of Primary Level 1 ,2 and 3 IPv4 and IPv6 dns server IPs for connected client");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"STEP 1 : Enable and verify the XDNS feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");

		LOGGER.info("STEP 2 : Obtain a working Connected client associated with the device");
		LOGGER.info(
				"STEP 3 : Verify if connected client is able to access the internet by pinging the site : 'www.google.com'");
		LOGGER.info(
				"STEP 4 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : Wi-Fi MacAddress of the connected client");
		LOGGER.info(
				"STEP 5 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net on the connected client");
		LOGGER.info(
				"STEP 6 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : Wi-Fi MacAddress ofthe connected client ");
		LOGGER.info(
				"STEP 7 : Verify xdns override at client mac Level by validating the domain name of the site : www.plannedparenthood.org on the connected client");
		LOGGER.info(
				"STEP 8 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : Wi-Fi MacAddress of the connected client");
		LOGGER.info(
				"STEP 9 : Verify xdns override at client mac Level by validating the domain name of the site : www.ar15.com on the connected client");
		LOGGER.info("STEP 10 : Factory Reset the device and verify if device comes up");
		LOGGER.info("STEP 11 : Set and verify the Global DNS IPv4 value ");
		LOGGER.info(
				"STEP 12 : Verify Captive portal status of the router after factory reset and connect the client settop to the SSID after reactivating the router.");
		LOGGER.info(
				"STEP 13 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net, After Factory resetting the router");
		LOGGER.info(
				"STEP 14 : Verify xdns override at client mac Level by validating the domain name of the site : www.plannedparenthood.org, After Factory resetting the router");
		LOGGER.info(
				"STEP 15 : Verify xdns override at client mac Level by validating the domain name of the site : www.ar15.com, After Factory resetting the router");
		LOGGER.info("#######################################################################################");
		// Holds the test case ID
		String testCaseId = "TC-RDKB-XDNS-102";
		// boolean variable to store the status
		boolean status = false;
		// Test step number
		String stepNumber = null;
		// Error message
		String errorMessage = null;
		try {
			/**
			 * PRE-CONDITIONS
			 */
			executePreConditions(device, testCaseId, true);
			/**
			 * "s1"
			 */
			// Test step number
			stepNumber = "s1";
			// boolean variable to store the status
			status = false;
			setAndVerifyXdnsFeature(device, testCaseId, stepNumber, true);
			/**
			 * "s2", "s3"
			 */
			// Test step number
			stepNumber = "s2";
			// boolean variable to store the status
			status = false;
			Dut connectedClientSettop = verifyConnectedClientAndInternetAccess(device, testCaseId,
					new String[] { "s2", "s3" });

			/**
			 * "s4"
			 */
			// Test step number
			stepNumber = "s4";
			// boolean variable to store the status
			status = false;
			// Mac address of the connected client
			String clientMacAddress = ((Device) connectedClientSettop).getConnectedDeviceInfo().getWifiMacAddress();
			applyDnsoverrideByAddingDNSMappingTable(device,
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
					clientMacAddress, testCaseId, "s4");
			/**
			 * "s5"
			 */
			// step number
			stepNumber = "s5";
			// boolean variable to store the status
			status = false;
			LOGGER.info("Waiting for 10 Seconds...");
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			// string to store site address for level one site from properties
			// file
			String addressForLevelOneSite = AutomaticsTapApi
					.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_HOST_ADDRESS_FOR_LEVEL_ONE_SITE);
			// string to store dns block site address for level one site from
			// properties file
			String dnsBlockAddressForLevelOneSite = AutomaticsTapApi
					.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_DNS_BLOCK_ADDRESS_FOR_LEVEL_ONE_SITE);
			// Connectivity check for Level One site
			verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelOneSite,
					dnsBlockAddressForLevelOneSite, testCaseId, "s5", false);
			/**
			 * "s6"
			 */
			// Test step number
			stepNumber = "s6";
			// boolean variable to store the status
			status = false;
			applyDnsoverrideByAddingDNSMappingTable(device,
					BroadBandTestConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_TWO_PRIMARY,
					BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_TWO_PRIMARY, clientMacAddress,
					testCaseId, "s6");
			/**
			 * "s7"
			 */
			// Test step number
			stepNumber = "s7";
			// boolean variable to store the status
			status = false;
			LOGGER.info("Waiting for 10 Seconds...");
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			// string to store site address for level two site from properties
			// file
			String addressForLevelTwoSite = AutomaticsTapApi
					.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_HOST_ADDRESS_FOR_LEVEL_TWO_SITE);
			// string to store dns block site address for level two site from
			// properties file
			String dnsBlockAddressForLevelTwoSite = AutomaticsTapApi
					.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_DNS_BLOCK_ADDRESS_FOR_LEVEL_TWO_SITE);
			// Connectivity check for Level Two site
			verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelTwoSite,
					dnsBlockAddressForLevelTwoSite, testCaseId, "s7", false);
			/**
			 * "s8"
			 */
			// Test step number
			stepNumber = "s8";
			// boolean variable to store the status
			status = false;
			applyDnsoverrideByAddingDNSMappingTable(device,
					AutomaticsPropertyUtility.getProperty(
							BroadBandPropertyKeyConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_THREE_PRIMARY),
					AutomaticsPropertyUtility.getProperty(
							BroadBandPropertyKeyConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_THREE_PRIMARY),
					clientMacAddress, testCaseId, "s8");
			/**
			 * "s9"
			 */
			// Test step number
			stepNumber = "s9";
			// boolean variable to store the status
			status = false;
			LOGGER.info("Waiting for 10 Seconds...");
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			// string to store site address for level three site from properties
			// file
			String addressForLevelThreeSite = AutomaticsTapApi
					.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_HOST_ADDRESS_FOR_LEVEL_THREE_SITE);
			// string to store dns block site address for level three site from
			// properties file
			String dnsBlockAddressForLevelThreeSite = AutomaticsTapApi
					.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_DNS_BLOCK_ADDRESS_FOR_LEVEL_THREE_SITE);
			// Connectivity check for Level Three site
			verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelThreeSite,
					dnsBlockAddressForLevelThreeSite, testCaseId, "s9", true);

			/**
			 * "s10"
			 */
			// Test step number
			stepNumber = "s10";
			// boolean variable to store the status
			status = false;
			executeFactoryResetOnTheDevice(device, testCaseId, stepNumber);

			/**
			 * "s11"
			 */
			// Test step number
			stepNumber = "s11";
			// boolean variable to store the status
			status = false;
			setAndVerifyValuesToGblDNSIPv4IPv6(device, testCaseId, "s11");

			/**
			 * "s12"
			 */
			// Test step number
			stepNumber = "s12";
			// boolean variable to store the status
			status = false;
			String currentPartnerIdName = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);

			if (BroadBandCommonUtils.verifySpecificSyndicationPartnerAvailability(currentPartnerIdName)) {
				verifyCaptivePrtlModeToReactivateRtrAndConnctClient(device, connectedClientSettop, testCaseId,
						stepNumber);
			} else {
				errorMessage = "This step is not applicable for specific Syndication Partner";
				LOGGER.info("STEP " + stepNumber + " - ACTUAL: " + errorMessage);
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * "s13"
			 */
			// Test step number
			stepNumber = "s13";
			// boolean variable to store the status
			status = false;
			// Connectivity check for Level One site
			verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelOneSite,
					addressForLevelOneSite, testCaseId, "s13", true);
			/**
			 * "s14"
			 */
			// Test step number
			stepNumber = "s14";
			// boolean variable to store the status
			status = false;
			// Connectivity check for Level Two site
			verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelTwoSite,
					addressForLevelTwoSite, testCaseId, "s14", true);
			/**
			 * "s15"
			 */
			// Test step number
			stepNumber = "s15";
			// boolean variable to store the status
			status = false;
			// Connectivity check for Level Three site
			verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelThreeSite,
					addressForLevelThreeSite, testCaseId, "s15", true);

		} catch (TestException exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("TC-RDKB-XDNS-1002 : Execution error occured due to exception --> " + exception.getMessage());
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
		} finally {
			try {
				BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
				BroadBandPostConditionUtils.executePostConditionForXdns(device, tapEnv, true);
				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			} catch (Exception exception) {
				LOGGER.error(
						"TC-RDKB-XDNS-1002 : Execution error occurred while executing post conditions due to exception --> "
								+ exception.getMessage());
			}
		}
	}

	/**
	 *
	 * Test Case : Verify if Skipping either one of the DNS server IP configuration
	 * -LEVEL 1 ,2 and 3 Primary Servers
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1: Connect the client setup to 2.4/5 GHZ SSID and verify
	 * connection status</li>
	 * <li>PRE-CONDITION 2: Verify the correct IPv4 address for client connected
	 * with 2.4/5 GHz SSID</li>
	 * <li>PRE-CONDITION 3: Verify the correct IPv6 address for client connected
	 * with 2.4/5 GHz SSID</li>
	 * <li>PRE-CONDITION 4: Verify the internet connectivity in the connected wifi
	 * client using ipv4 interface</li>
	 * <li>PRE-CONDITION 5: Verify the internet connectivity in the connected wifi
	 * client using ipv6 interface.</li>
	 * <li>PRE-CONDITION 6: Enable and verify the XDNS feature</li>
	 * <li>PRE-CONDITION 7: Set and verify the Global DNS IPv4 value to
	 * '75.75.75.75'</li>
	 * <li>PRE-CONDITION 8: Set and verify the Global DNS IPv6 value to
	 * '2001:558:feed::1'</li>
	 * <li>Step 1: Add the device details in mapping table for Level 1 Primary
	 * Server DnsIPv4 and verify the Verify the presence of the dnsmasq_servers.conf
	 * file in the device</li>
	 * <li>Step 2: Verify low level blocked site www.seriesw.net is accessible on
	 * level 1 primary server DnsIPv4</li>
	 * <li>Step 3: Verify medium level blocked site www.plannedparenthood.org is
	 * accessible on level 1 primary server DnsIPv4</li>
	 * <li>Step 4: Verify high level blocked site www.ar15.com is accessible on
	 * level 1 primary server DnsIPv4</li>
	 * <li>Step 5: Add the device details in mapping table for Level 1 Primary
	 * Server DnsIPv6 and verify the Verify the presence of the dnsmasq_servers.conf
	 * file in the device</li>
	 * <li>Step 6: Verify low level blocked site www.seriesw.net is accessible on
	 * level 1 primary server DnsIPv6</li>
	 * <li>Step 7: Verify medium level blocked site www.plannedparenthood.org is
	 * accessible on level 1 primary server DnsIPv6</li>
	 * <li>Step 8: Verify high level blocked site www.ar15.com is accessible on
	 * level 1 primary server DnsIPv6</li>
	 * <li>Step 9: Add the device details in mapping table for Level 2 Primary
	 * Server DnsIPv4 and verify the Verify the presence of the dnsmasq_servers.conf
	 * file in the device</li>
	 * <li>Step 10: Verify low level blocked site www.seriesw.net is accessible on
	 * level 2 primary server DnsIPv4</li>
	 * <li>Step 11: Verify medium level blocked site www.plannedparenthood.org is
	 * accessible on level 2 primary server DnsIPv4</li>
	 * <li>Step 12: Verify high level blocked site www.ar15.com is accessible on
	 * level 2 primary server DnsIPv4</li>
	 * <li>Step 13: Add the device details in mapping table for Level 2 Primary
	 * Server DnsIPv6 and verify the Verify the presence of the dnsmasq_servers.conf
	 * file in the device</li>
	 * <li>Step 14: Verify low level blocked site www.seriesw.net is accessible on
	 * level 2 primary server DnsIPv6</li>
	 * <li>Step 15: Verify medium level blocked site www.plannedparenthood.org is
	 * accessible on level 2 primary server DnsIPv6</li>
	 * <li>Step 16: Verify high level blocked site www.ar15.com is accessible on
	 * level 2 primary server DnsIPv6</li>
	 * <li>Step 17: Add the device details in mapping table for Level 3 Primary
	 * Server DnsIPv4 and verify the Verify the presence of the dnsmasq_servers.conf
	 * file in the device</li>
	 * <li>Step 18: Verify low level blocked site www.seriesw.net is accessible on
	 * level 3 primary server DnsIPv4</li>
	 * <li>Step 19: Verify medium level blocked site www.plannedparenthood.org is
	 * accessible on level 3 primary server DnsIPv4</li>
	 * <li>Step 20: Verify high level blocked site www.ar15.com is accessible on
	 * level 3 primary server DnsIPv4</li>
	 * <li>Step 21: Add the device details in mapping table for Level 3 Primary
	 * Server DnsIPv6 and verify the Verify the presence of the dnsmasq_servers.conf
	 * file in the device</li>
	 * <li>Step 22: Verify low level blocked site www.seriesw.net is accessible on
	 * level 3 primary server DnsIPv6</li>
	 * <li>Step 23: Verify medium level blocked site www.plannedparenthood.org is
	 * accessible on level 3 primary server DnsIPv6</li>
	 * <li>Step 24: Verify high level blocked site www.ar15.com is accessible on
	 * level 3 primary server DnsIPv6</li>
	 * <li>POST-CONDITION 1: Disable and verify the XDNS feature</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 * @refactor Govardhan
	 **/
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-XDNS-PRI-ALL-LVL-5001")
	public void testToVerifyPrimaryAllLevelDnsServerConfigSkipIPv4OrIPv6(Dut device) {
		String testCaseId = "TC-RDKB-XDNS-PRI-ALL-LVL-501";
		Dut deviceConnected = null;
		int stepNumber = 1;

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-XDNS-PRI-ALL-LVL-5001");
		LOGGER.info(
				"TEST DESCRIPTION: Verify if Skipping either one of the DNS server IP configuration -LEVEL 1 ,2 and 3 Primary Servers");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1: Connect the client setup to 2.4/5 GHZ SSID and verify connection status");
		LOGGER.info("PRE-CONDITION 2: Verify the correct IPv4 address for client connected with 2.4/5 GHz SSID");
		LOGGER.info("PRE-CONDITION 3: Verify the correct IPv6 address for client connected with 2.4/5 GHz SSID");
		LOGGER.info(
				"PRE-CONDITION 4: Verify the internet connectivity in the connected wifi client using ipv4 interface");
		LOGGER.info(
				"PRE-CONDITION 5: Verify the internet connectivity in the connected wifi client using ipv6 interface.");
		LOGGER.info("PRE-CONDITION 6: Enable and verify the XDNS feature");
		LOGGER.info("PRE-CONDITION 7: Set and verify the Global DNS IPv4 value to '75.75.75.75'");
		LOGGER.info("PRE-CONDITION 8: Set and verify the Global DNS IPv6 value to '2001:558:feed::1'");
		LOGGER.info(
				"Step 1: Add the device details in mapping table for Level 1 Primary Server DnsIPv4 and verify the Verify the presence of the dnsmasq_servers.conf file in the device");
		LOGGER.info(
				"Step 2: Verify low level blocked site www.seriesw.net is accessible on level 1 primary server DnsIPv4");
		LOGGER.info(
				"Step 3: Verify medium level blocked site www.plannedparenthood.org is accessible on level 1 primary server DnsIPv4");
		LOGGER.info(
				"Step 4: Verify high level blocked site www.ar15.com is accessible on level 1 primary server DnsIPv4");
		LOGGER.info(
				"Step 5: Add the device details in mapping table for Level 1 Primary Server DnsIPv6 and verify the Verify the presence of the dnsmasq_servers.conf file in the device");
		LOGGER.info(
				"Step 6: Verify low level blocked site www.seriesw.net is accessible on level 1 primary server DnsIPv6");
		LOGGER.info(
				"Step 7: Verify medium level blocked site www.plannedparenthood.org is accessible on level 1 primary server DnsIPv6");
		LOGGER.info(
				"Step 8: Verify high level blocked site www.ar15.com is accessible on level 1 primary server DnsIPv6");
		LOGGER.info(
				"Step 9: Add the device details in mapping table for Level 2 Primary Server DnsIPv4 and verify the Verify the presence of the dnsmasq_servers.conf file in the device");
		LOGGER.info(
				"Step 10: Verify low level blocked site www.seriesw.net is accessible on level 2 primary server DnsIPv4");
		LOGGER.info(
				"Step 11: Verify medium level blocked site www.plannedparenthood.org is accessible on level 2 primary server DnsIPv4");
		LOGGER.info(
				"Step 12: Verify high level blocked site www.ar15.com is accessible on level 2 primary server DnsIPv4");
		LOGGER.info(
				"Step 13: Add the device details in mapping table for Level 2 Primary Server DnsIPv6 and verify the Verify the presence of the dnsmasq_servers.conf file in the device");
		LOGGER.info(
				"Step 14: Verify low level blocked site www.seriesw.net is accessible on level 2 primary server DnsIPv6");
		LOGGER.info(
				"Step 15: Verify medium level blocked site www.plannedparenthood.org is accessible on level 2 primary server DnsIPv6");
		LOGGER.info(
				"Step 16: Verify high level blocked site www.ar15.com is accessible on level 2 primary server DnsIPv6");
		LOGGER.info(
				"Step 17: Add the device details in mapping table for Level 3 Primary Server DnsIPv4 and verify the Verify the presence of the dnsmasq_servers.conf file in the device");
		LOGGER.info(
				"Step 18: Verify low level blocked site www.seriesw.net is accessible on level 3 primary server DnsIPv4");
		LOGGER.info(
				"Step 19: Verify medium level blocked site www.plannedparenthood.org is accessible on level 3 primary server DnsIPv4");
		LOGGER.info(
				"Step 20: Verify high level blocked site www.ar15.com is accessible on level 3 primary server DnsIPv4");
		LOGGER.info(
				"Step 21: Add the device details in mapping table for Level 3 Primary Server DnsIPv6 and verify the Verify the presence of the dnsmasq_servers.conf file in the device");
		LOGGER.info(
				"Step 22: Verify low level blocked site www.seriesw.net is accessible on level 3 primary server DnsIPv6");
		LOGGER.info(
				"Step 23: Verify medium level blocked site www.plannedparenthood.org is accessible on level 3 primary server DnsIPv6");
		LOGGER.info(
				"Step 24: Verify high level blocked site www.ar15.com is accessible on level 1 primary server DnsIPv6");
		LOGGER.info("POST-CONDITION 1: Disable and verify the XDNS feature");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			deviceConnected = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device, tapEnv,
					BroadBandTestConstants.BAND_2_4GHZ_OR_5GHZ);
			/**
			 * PRECONDITION 6 : ENABLE AND VERIFY THE XDNS FEATURE
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 6 : DESCRIPTION : ENABLE AND VERIFY THE XDNS FEATURE");
			LOGGER.info("PRE-CONDITION 6 : ACTION : ENABLE AND VERIFY THE XDNS FEATURE USING WEBPA");
			LOGGER.info("PRE-CONDITION 6 : EXPECTED : XDNS FEATURE MUST BE ENABLED SUCESSFULLY ");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO ENABLE THE XDNS FEATURE";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE, RDKBTestConstants.THREE_MINUTES,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("PRE-CONDITION 6 : ACTUAL : XDNS FEATURE ENABLED SUCCESSFULLY.");
			} else {
				LOGGER.error("PRE-CONDITION 6 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 6 : FAILED : " + errorMessage);
			}

			/**
			 * PRECONDITION 7 : SET AND VERIFY THE GLOBAL DNS IPV4 VALUE TO <IPv4>
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 7 : DESCRIPTION : SET AND VERIFY THE GLOBAL DNS IPV4 VALUE TO "
					+ BroadbandPropertyFileHandler.getGlobalDNSIpv4Value());
			LOGGER.info("PRE-CONDITION 7 : ACTION : SET AND VERIFY THE GLOBAL DNS IPV4 VALUE TO "
					+ BroadbandPropertyFileHandler.getGlobalDNSIpv4Value() + " USING WEBPA");
			LOGGER.info("PRE-CONDITION 7 : EXPECTED : MUST SET AND VERIFY THE GLOBAL DNS IPV4 VALUE TO "
					+ BroadbandPropertyFileHandler.getGlobalDNSIpv4Value());
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET AND VERIFY THE GLOBAL DNS IPV4 VALUE TO "
					+ BroadbandPropertyFileHandler.getGlobalDNSIpv4Value();
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, WebPaDataTypes.STRING.getValue(),
					BroadbandPropertyFileHandler.getGlobalDNSIpv4Value(), RDKBTestConstants.THREE_MINUTES,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("PRE-CONDITION 7 : ACTUAL : SUCCESSFULLY SET AND VERIFIED THE GLOBAL DNS IPV4 VALUE TO "
						+ BroadbandPropertyFileHandler.getGlobalDNSIpv4Value());
			} else {
				LOGGER.error("PRE-CONDITION 7 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 7 : FAILED : " + errorMessage);
			}
			/**
			 * PRECONDITION 8 : SET AND VERIFY THE GLOBAL DNS IPV6 VALUE TO <IPv6>
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 8 : DESCRIPTION : SET AND VERIFY THE GLOBAL DNS IPV6 VALUE TO "
					+ BroadbandPropertyFileHandler.getGlobalDNSIpv6Value());
			LOGGER.info("PRE-CONDITION 8 : ACTION : SET AND VERIFY THE GLOBAL DNS IPV6 VALUE TO "
					+ BroadbandPropertyFileHandler.getGlobalDNSIpv6Value() + " USING WEBPA");
			LOGGER.info("PRE-CONDITION 8 : EXPECTED : MUST SET AND VERIFY THE GLOBAL DNS IPV6 VALUE TO "
					+ BroadbandPropertyFileHandler.getGlobalDNSIpv6Value());
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET AND VERIFY THE GLOBAL DNS IPV6 VALUE TO "
					+ BroadbandPropertyFileHandler.getGlobalDNSIpv6Value();
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6, WebPaDataTypes.STRING.getValue(),
					BroadbandPropertyFileHandler.getGlobalDNSIpv6Value(), RDKBTestConstants.THREE_MINUTES,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("PRE-CONDITION 8 : ACTUAL : SUCCESSFULLY SET AND VERIFIED THE GLOBAL DNS IPV6 VALUE TO "
						+ BroadbandPropertyFileHandler.getGlobalDNSIpv6Value());
			} else {
				LOGGER.error("PRE-CONDITION 8 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 8 : FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1 : ADD THE DEVICE DETAILS IN MAPPING TABLE FOR LEVEL 1 PRIMARY SERVER
			 * DNS IPV4 AND VERIFY THE VERIFY THE PRESENCE OF THE DNSMASQ_SERVERS.CONF FILE
			 * IN THE DEVICE
			 */
			addDeviceDetailsMappingInDnsServer(device, deviceConnected,
					BroadbandPropertyFileHandler.getDNSIpv4ValueLevelOnePrimary(),
					BroadBandTestConstants.STRING_DNS_IPV4, stepNumber, testCaseId,
					BroadBandTestConstants.STRING_CONSTANT_1);

			/**
			 * Step 2-4 : VERIFY ALL LEVEL BLOCKED SITES ARE ACCESSIBLE ON LEVEL 1 PRIMARY
			 * SERVER DNSIPV4
			 */
			stepNumber++;
			performNsLookUpOnConnectedDevice(device, deviceConnected,
					BroadbandPropertyFileHandler.getGlobalDNSIpv4Value(), BroadBandTestConstants.STRING_DNS_IPV4,
					stepNumber, testCaseId, BroadBandTestConstants.STRING_CONSTANT_1,
					BroadBandTestConstants.LOW_LEVEL_SITE_BLOCKER);

			/**
			 * Step 5 : ADD THE DEVICE DETAILS IN MAPPING TABLE FOR LEVEL 1 PRIMARY SERVER
			 * DNSIPV6 AND VERIFY THE VERIFY THE PRESENCE OF THE DNSMASQ_SERVERS.CONF FILE
			 * IN THE DEVICE
			 */
			stepNumber = 5;
			addDeviceDetailsMappingInDnsServer(device, deviceConnected,
					BroadbandPropertyFileHandler.getDNSIpv6ValueLevelOnePrimary(),
					BroadBandTestConstants.STRING_DNS_IPV6, stepNumber, testCaseId,
					BroadBandTestConstants.STRING_CONSTANT_1);

			/**
			 * Step 6-8 : VERIFY ALL LEVEL BLOCKED SITES ARE ACCESSIBLE ON LEVEL 1 PRIMARY
			 * SERVER DNSIPV6
			 */
			stepNumber++;
			performNsLookUpOnConnectedDevice(device, deviceConnected,
					BroadbandPropertyFileHandler.getGlobalDNSIpv6Value(), BroadBandTestConstants.STRING_DNS_IPV6,
					stepNumber, testCaseId, BroadBandTestConstants.STRING_CONSTANT_1,
					BroadBandTestConstants.LOW_LEVEL_SITE_BLOCKER);

			/**
			 * Step 9 : ADD THE DEVICE DETAILS IN MAPPING TABLE FOR LEVEL 2 PRIMARY SERVER
			 * DNS IPV4 AND VERIFY THE VERIFY THE PRESENCE OF THE DNSMASQ_SERVERS.CONF FILE
			 * IN THE DEVICE
			 */
			stepNumber = 9;
			addDeviceDetailsMappingInDnsServer(device, deviceConnected,
					BroadbandPropertyFileHandler.getDNSIpv4ValueLevelTwoPrimary(),
					BroadBandTestConstants.STRING_DNS_IPV4, stepNumber, testCaseId,
					BroadBandTestConstants.STRING_CONSTANT_1);

			/**
			 * Step 10-12 : VERIFY ALL LEVEL BLOCKED SITES ARE ACCESSIBLE ON LEVEL 2 PRIMARY
			 * SERVER DNSIPV4
			 */
			stepNumber++;
			performNsLookUpOnConnectedDevice(device, deviceConnected,
					BroadbandPropertyFileHandler.getGlobalDNSIpv4Value(), BroadBandTestConstants.STRING_DNS_IPV4,
					stepNumber, testCaseId, BroadBandTestConstants.STRING_CONSTANT_1,
					BroadBandTestConstants.MEDIUM_LEVEL_SITE_BLOCKER);

			/**
			 * Step 13 : ADD THE DEVICE DETAILS IN MAPPING TABLE FOR LEVEL 2 PRIMARY SERVER
			 * DNSIPV6 AND VERIFY THE VERIFY THE PRESENCE OF THE DNSMASQ_SERVERS.CONF FILE
			 * IN THE DEVICE
			 */
			stepNumber = 13;
			addDeviceDetailsMappingInDnsServer(device, deviceConnected,
					BroadbandPropertyFileHandler.getDNSIpv6ValueLevelTwoPrimary(),
					BroadBandTestConstants.STRING_DNS_IPV6, stepNumber, testCaseId,
					BroadBandTestConstants.STRING_CONSTANT_1);

			/**
			 * Step 14-16 : VERIFY ALL LEVEL BLOCKED SITES ARE ACCESSIBLE ON LEVEL 1 PRIMARY
			 * SERVER DNSIPV6
			 */
			stepNumber++;
			performNsLookUpOnConnectedDevice(device, deviceConnected,
					BroadbandPropertyFileHandler.getGlobalDNSIpv6Value(), BroadBandTestConstants.STRING_DNS_IPV6,
					stepNumber, testCaseId, BroadBandTestConstants.STRING_CONSTANT_1,
					BroadBandTestConstants.MEDIUM_LEVEL_SITE_BLOCKER);

			/**
			 * Step 17 : ADD THE DEVICE DETAILS IN MAPPING TABLE FOR LEVEL 3 PRIMARY SERVER
			 * DNS IPV4 AND VERIFY THE VERIFY THE PRESENCE OF THE DNSMASQ_SERVERS.CONF FILE
			 * IN THE DEVICE
			 */
			stepNumber = 17;
			addDeviceDetailsMappingInDnsServer(device, deviceConnected,
					BroadbandPropertyFileHandler.getDNSIpv4ValueLevelThreePrimary(),
					BroadBandTestConstants.STRING_DNS_IPV4, stepNumber, testCaseId,
					BroadBandTestConstants.STRING_CONSTANT_1);

			/**
			 * Step 18-20 : VERIFY ALL LEVEL BLOCKED SITES ARE ACCESSIBLE ON LEVEL 3 PRIMARY
			 * SERVER DNSIPV4
			 */
			stepNumber++;
			performNsLookUpOnConnectedDevice(device, deviceConnected,
					BroadbandPropertyFileHandler.getGlobalDNSIpv4Value(), BroadBandTestConstants.STRING_DNS_IPV4,
					stepNumber, testCaseId, BroadBandTestConstants.STRING_CONSTANT_1,
					BroadBandTestConstants.HIGH_LEVEL_SITE_BLOCKER);

			/**
			 * Step 21 : ADD THE DEVICE DETAILS IN MAPPING TABLE FOR LEVEL 3 PRIMARY SERVER
			 * DNSIPV6 AND VERIFY THE VERIFY THE PRESENCE OF THE DNSMASQ_SERVERS.CONF FILE
			 * IN THE DEVICE
			 */
			stepNumber = 21;
			addDeviceDetailsMappingInDnsServer(device, deviceConnected,
					BroadbandPropertyFileHandler.getDNSIpv6ValueLevelThreePrimary(),
					BroadBandTestConstants.STRING_DNS_IPV6, stepNumber, testCaseId,
					BroadBandTestConstants.STRING_CONSTANT_1);

			/**
			 * Step 22-24 : VERIFY ALL LEVEL BLOCKED SITES ARE ACCESSIBLE ON LEVEL 1 PRIMARY
			 * SERVER DNSIPV6
			 */
			stepNumber++;
			performNsLookUpOnConnectedDevice(device, deviceConnected,
					BroadbandPropertyFileHandler.getGlobalDNSIpv6Value(), BroadBandTestConstants.STRING_DNS_IPV6,
					stepNumber, testCaseId, BroadBandTestConstants.STRING_CONSTANT_1,
					BroadBandTestConstants.HIGH_LEVEL_SITE_BLOCKER);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE VERIFYING EITHER ONE OF THE DNS SERVER IP CONFIGURATION -LEVEL 1 ,2 AND 3 PRIMARY SERVERS :"
							+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			BroadBandPostConditionUtils.executePostConditionToDisableXdnsStatus(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			if (deviceConnected != null) {
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION 2 : DESCRIPTION : DISCONNECT WIFI RADIO 2.4GHZ/5GHZ SSID FROM THE DEVICE");
				LOGGER.info("POST-CONDITION 2 : ACTION : DISCONNECT WIFI RADIO 2.4GHZ/5GHZ SSID ");
				LOGGER.info(
						"POST-CONDITION 2 : EXPECTED : PRIVATE WIFI 2.4GHZ/5GHZ SSID SHOULD BE DISCONNECTED SUCCESSFULLY");
				LOGGER.info("#######################################################################################");
				status = false;
				try {
					BroadBandResultObject resultObject = BroadBandConnectedClientUtils
							.disconnectCnnClientFromSsid(tapEnv, device, deviceConnected);
					status = resultObject.isStatus();
					errorMessage = resultObject.getErrorMessage();
				} catch (Exception e) {
					errorMessage = e.getMessage();
					LOGGER.error("EXCEPTION OCCURRED WHILE PERFORMING POST CONDITION 1" + errorMessage);
				}
				if (status) {
					LOGGER.info("POST-CONDITION 2 : ACTUAL : PRIVATE WIFI 2.4GHZ/5GHZ SSID DISCONNECTED SUCCESSFULLY");
				} else {
					LOGGER.error("POST-CONDITION 2 : ACTUAL : " + errorMessage);
				}
			}
			LOGGER.info("########################### ENDING POST CONFIGURATION ####################################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-XDNS-PRI-ALL-LVL-5001");
	}

	/**
	 * This method is to verify if the blocked site is accessible on the dns server
	 * by checking the DNS Configuration
	 * 
	 * @param device          instance of {@link Dut}
	 * @param deviceConnected instance of {@link ConnectedClient}
	 * @param dnsServerIp     DNS server IP Address
	 * @param dnsServerIpType DNS Server Ip Address Type(IPv4/IPv6)
	 * @param stepNumber      Step Number
	 * @param testCaseId      Test case Id
	 * @param primaryLevel    Primary level
	 * @param siteBlocker     DNS Site blocker for
	 * @refactor Govardhan
	 */
	private static void performNsLookUpOnConnectedDevice(Dut device, Dut deviceConnected, String dnsServerIp,
			String dnsServerIpType, int stepNumber, String testCaseId, String primaryLevel, String siteBlocker) {
		/**
		 * Step : VERIFY LOW LEVEL BLOCKED SITE WWW.SERIESW.NET IS ACCESSIBLE ON PRIMARY
		 * SERVER AND RESPECTIVE DNSIP TYPE
		 */
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY LOW LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.LOW_LEVEL_SITE)
				+ " IS ACCESSIBLE ON LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType);
		LOGGER.info("STEP " + stepNumber + " : ACTION : PERFORM NSLOOKUP FOR LOW LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.LOW_LEVEL_SITE)
				+ " ON LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType + ": EXECUTE COMMAND : nslookup "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.LOW_LEVEL_SITE) + " "
				+ dnsServerIp);
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : LOW LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.LOW_LEVEL_SITE)
				+ " MUST BE ACCESSIBLE ON LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType);
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO ACCESS THE LOW LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.LOW_LEVEL_SITE)
				+ " IN LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType;
		status = BroadBandConnectedClientUtils.verifySiteNsLookUpOnCnctdClient(tapEnv, deviceConnected,
				BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.LOW_LEVEL_SITE), dnsServerIp,
				BroadBandTestConstants.DNS_SITE_BLOCKER_MAPPING.get(siteBlocker));
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : LOW LEVEL BLOCKED SITE "
					+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.LOW_LEVEL_SITE)
					+ " IS ACCESSIBLE SUCCESSFULLY IN LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		/**
		 * Step : VERIFY LOW LEVEL BLOCKED WWW.PLANNEDPARENTHOOD.ORG IS ACCESSIBLE ON
		 * PRIMARY SERVER AND RESPECTIVE DNSIP TYPE
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY MEDIUM LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.MEDIUM_LEVEL_SITE)
				+ " IS ACCESSIBLE ON LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType);
		LOGGER.info("STEP " + stepNumber + " : ACTION : PERFORM NSLOOKUP FOR MEDIUM LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.MEDIUM_LEVEL_SITE)
				+ " ON LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType + ": EXECUTE COMMAND : nslookup "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.MEDIUM_LEVEL_SITE) + " "
				+ dnsServerIp);
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : MEDIUM LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.MEDIUM_LEVEL_SITE)
				+ " MUST BE ACCESSIBLE ON LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType);
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO ACCESS THE MEDIUM LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.MEDIUM_LEVEL_SITE)
				+ " IN LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType;
		status = BroadBandConnectedClientUtils.verifySiteNsLookUpOnCnctdClient(tapEnv, deviceConnected,
				BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.MEDIUM_LEVEL_SITE),
				dnsServerIp, BroadBandTestConstants.DNS_SITE_BLOCKER_MAPPING.get(siteBlocker));
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : MEDIUM LEVEL BLOCKED SITE "
					+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.MEDIUM_LEVEL_SITE)
					+ " IS ACCESSIBLE SUCCESSFULLY IN LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		/**
		 * Step : VERIFY LOW LEVEL BLOCKED SITE WWW.AR15.COM IS ACCESSIBLE ON PRIMARY
		 * SERVER AND RESPECTIVE DNSIP TYPE
		 */
		stepNumber++;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY HIGH LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.HIGH_LEVEL_SITE)
				+ " IS ACCESSIBLE ON LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType);
		LOGGER.info("STEP " + stepNumber + " : ACTION : PERFORM NSLOOKUP FOR HIGH LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.HIGH_LEVEL_SITE)
				+ " ON LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType + ": EXECUTE COMMAND : nslookup "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.HIGH_LEVEL_SITE) + " "
				+ dnsServerIp);
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : HIGH LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.HIGH_LEVEL_SITE)
				+ " MUST BE ACCESSIBLE ON LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType);
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO ACCESS THE HIGH LEVEL BLOCKED SITE "
				+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.HIGH_LEVEL_SITE)
				+ " IN LEVEL " + primaryLevel + " PRIMARY SERVER " + dnsServerIpType;
		status = BroadBandConnectedClientUtils.verifySiteNsLookUpOnCnctdClient(tapEnv, deviceConnected,
				BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.HIGH_LEVEL_SITE), dnsServerIp,
				BroadBandTestConstants.DNS_SITE_BLOCKER_MAPPING.get(siteBlocker));
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : HIGH LEVEL BLOCKED SITE "
					+ BroadBandTestConstants.RESTRICTED_SITE_MAPPING.get(BroadBandTestConstants.HIGH_LEVEL_SITE)
					+ " IS ACCESSIBLE SUCCESSFULLY IN LEVEL" + primaryLevel + " PRIMARY SERVER " + dnsServerIpType);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	}

	/**
	 * Method to add the DNS mapping table and verify the entry in
	 * dnsmasq_servers.conf
	 * 
	 * @param device          instance of {@link Dut}
	 * @param deviceConnected instance of {@link ConnectedClient}
	 * @param dnsServerIp     DNS server IP Address
	 * @param dnsServerIpType DNS Server Ip Address Type(IPv4/IPv6)
	 * @param stepNumber      Step Number
	 * @param testCaseId      Test case Id
	 * @param primaryLevel    Primary level
	 * @refactor Govardhan
	 */
	private static void addDeviceDetailsMappingInDnsServer(Dut device, Dut deviceConnected, String dnsServerIp,
			String dnsServerIpType, int stepNumber, String testCaseId, String primaryLevel) {
		BroadBandResultObject resultObject = null;
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		/**
		 * Step : ADD THE DEVICE DETAILS IN MAPPING TABLE FOR RESPECITVE LEVEL PRIMARY
		 * SERVER DNS IP TYPE AND VERIFY THE PRESENCE OF THE DNSMASQ_SERVERS.CONF FILE
		 * IN THE DEVICE
		 */
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : ADD THE DEVICE DETAILS IN MAPPING TABLE FOR LEVEL "
				+ primaryLevel + " PRIMARY SERVER " + dnsServerIpType
				+ " AND VERIFY THE VERIFY THE PRESENCE OF THE DNSMASQ_SERVERS.CONF FILE IN THE DEVICE");
		LOGGER.info("STEP " + stepNumber + " : ACTION : ADD THE DEVICE DETAILS IN MAPPING TABLE FOR LEVEL "
				+ primaryLevel + " PRIMARY SERVER DNSIPV4 USING WEBPA PARAM :" + dnsServerIp);
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : DNS MAPPING TABLE SHOULD BE ADDED SUCCESSFULLY FOR LEVEL "
				+ primaryLevel + " PRIMARY SERVER DNSIPV4");
		LOGGER.info("#######################################################################################");
		errorMessage = "ATTEMPT TO APPLY DNSOVERRIDE THROUGH DNS MAPPING TABLE FAILED FOR LEVEL " + primaryLevel
				+ " PRIMARY SERVER DNSIPV4";
		resultObject = BroadBandConnectedClientUtils.addDnsMappingTable(device, tapEnv, deviceConnected, dnsServerIp,
				dnsServerIpType);
		status = resultObject.isStatus();
		errorMessage = resultObject.getErrorMessage();
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : DNS MAPPING TABLE ADDED SUCCESSFULLY FOR LEVEL "
					+ primaryLevel + " PRIMARY SERVER DNSIPV4");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	}

	/**
	 * Test to verify Add support for secondary xDNS server
	 * <ol>
	 * <li>Factory reset the device for default values</li>
	 * <li>Disabled captive portal and connect to the client</li>
	 * <li>verify GET of these parameters using WEBPA if they have default
	 * values</li>
	 * <li>Enable XDNS with default settings</li>
	 * <li>Check /etc/resolv.conf contains XDNS entries</li>
	 * <li>Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are
	 * sent to tcp dump when DNS requests in connected clients in browser.</li>
	 * <li>Set different values other than default for XDNS.set valid value for
	 * Primary and invalid value for Secondary address using dmcli/WEBPA</li>
	 * <li>Check /etc/resolv.conf contains XDNS entries</li>
	 * <li>Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are
	 * sent to primary XDNS servers when making DNS requests in connected clients in
	 * browser for Scenario1 and check browser is accessible.</li>
	 * <li>Set different values other than default for XDNS.set invalid value for
	 * Primary and valid value for Secondary address using dmcli/WEBPA</li>
	 * <li>Check /etc/resolv.conf contains XDNS entries</li>
	 * <li>Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are
	 * sent to secondary XDNS servers when making DNS requests in connected clients
	 * in browser for Scenario2 and check browser is accessible</li>
	 * <li>Set different values other than default for XDNS.set invalid value for
	 * Primary and invalid value for Secondary address using dmcli/WEBPA</li>
	 * <li>Check /etc/resolv.conf contains XDNS entries</li>
	 * <li>Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are
	 * sent to tcp dump when making DNS requests in connected clients in browser for
	 * Scenario3 and check browser is not accessible</li>
	 * <li>Set Secondary IPv4 and IPv6 address to NULL</li>
	 * <li>verify only primary address in /etc/resolv.conf contains XDNS
	 * entries</li>
	 * <li>Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are
	 * sent to primary XDNS servers when making DNS requests in connected clients in
	 * browser and check browser is accessible</li>
	 * <li>Disable XDNS</li>
	 * <li>verify resolv.conf does not contain any dnsoverride entries
	 * /etc/resolv.conf</li>
	 * <li>Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are
	 * sent to tcpdump when making DNS requests in connected clients in browser and
	 * check browser is accessible</li>
	 * <li>Check for \"XDNS_SetParamStringValue\" log after setting secondary values
	 * in log files</li>
	 * <li>EnableXDNS and check for log string \"Enabling secondary XDNS\"</li>
	 * 
	 * @author Betel Costrow
	 * @refactor Govardhan
	 *
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-XDNS-1006")
	public void testToVerifySecondaryXDNSServer(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-XDNS-106";
		String stepNum = "";
		String errorMessage = "";
		String response = null;
		boolean status = false;
		String tcpDumpWrite = null;
		String tcpDumpRead = null;
		String tcpDumpReadAndWrite = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-XDNS-1006");
		LOGGER.info("TEST DESCRIPTION: Test to verify Add support for secondary xDNS server");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1.Factory reset the device for default values");
		LOGGER.info("2.Disabled captive portal and connect to the client");
		LOGGER.info("3.verify GET of these parameters using WEBPA if they have default values");
		LOGGER.info("4.Enable XDNS with default settings");
		LOGGER.info("5.Check /etc/resolv.conf contains XDNS entries");
		LOGGER.info(
				"6.Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent  to tcp dump when DNS requests in connected clients in browser.");
		LOGGER.info(
				"7.Set different values other than default for XDNS.set valid value for Primary and invalid value for Secondary address using dmcli/WEBPA");
		LOGGER.info("8.Check /etc/resolv.conf contains XDNS entries");
		LOGGER.info(
				"9.Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent to primary XDNS servers when making DNS requests in connected clients in browser for Scenario1 and check browser is accessible.");
		LOGGER.info(
				"10.Set different values other than default for XDNS.set invalid value for Primary and valid value for Secondary address using dmcli/WEBPA");
		LOGGER.info("11.Check /etc/resolv.conf contains XDNS entries");
		LOGGER.info(
				"12.Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent to secondary XDNS servers when making DNS requests in connected clients in browser for Scenario2 and check browser is accessible");
		LOGGER.info(
				"13.Set different values other than default for XDNS.set invalid value for Primary and invalid value for Secondary address using dmcli/WEBPA");
		LOGGER.info("14.Check /etc/resolv.conf contains XDNS entries");
		LOGGER.info(
				"15.Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent to tcp dump when making DNS requests in connected clients in browser for Scenario3 and check browser is not accessible");
		LOGGER.info("16.Set Secondary IPv4 and IPv6 address to NULL");
		LOGGER.info("17.verify only primary address in /etc/resolv.conf contains XDNS entries");
		LOGGER.info(
				"18.Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent to  primary XDNS servers when making DNS requests in connected clients in browser and check browser is accessible");
		LOGGER.info("19.Disable XDNS");
		LOGGER.info("20.verify resolv.conf does not contain any dnsoverride entries /etc/resolv.conf");
		LOGGER.info(
				"21.Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent to tcpdump when making DNS requests in connected clients in browser and check browser is accessible");
		LOGGER.info("22.Check for \"XDNS_SetParamStringValue\" log after setting secondary values in log files");
		LOGGER.info("23.EnableXDNS and check for log string \"Enabling secondary XDNS\"");

		LOGGER.info("#######################################################################################");

		try {
			if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				tcpDumpWrite = BroadBandCommandConstants.CMD_TO_WRITE_TCPDUMP_DUMMY_FILE_ATOM_DEVICE;
				tcpDumpRead = BroadBandCommandConstants.CMD_TO_READ_TCPDUMP_DUMMY_FILE_ATOM_DEVICE;
				tcpDumpReadAndWrite = BroadBandCommandConstants.CMD_TO_WRITE_TCPDUMP_CAPTURE_FILE_ATOM_DEVICE;
			} else {
				tcpDumpWrite = BroadBandCommandConstants.CMD_TO_WRITE_TCPDUMP_DUMMY_FILE;
				tcpDumpRead = BroadBandCommandConstants.CMD_TO_READ_TCPDUMP_DUMMY_FILE;
				tcpDumpReadAndWrite = BroadBandCommandConstants.CMD_TO_WRITE_TCPDUMP_CAPTURE_FILE;
			}

			stepNum = "s1";
			errorMessage = "Not able to perform factory reset";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Factory reset the device for default values");
			LOGGER.info(
					"STEP 1: ACTION : Execute: dmcli eRT setv Device.X_CISCO_COM_DeviceControl.FactoryReset string Router,Wifi,VoIP,Dect,MoCA ");
			LOGGER.info("STEP 1: EXPECTED : Device should come with Factory reset");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully performed factory reset");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Unable to disable captive portal/wifi configuration and not able to connect client device";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Disabled captive portal and connect to the client");
			LOGGER.info(
					"STEP 2: ACTION : Execute below steps:dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_ConfigureWiFi bool false"
							+ "dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable bool false");
			LOGGER.info(
					"STEP 2: EXPECTED : Captive portal should get disabled and should be able to connect to client in webpage");
			LOGGER.info("**********************************************************************************");

			boolean disableCaptivePortal = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);
			boolean disableWifiConfigure = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_INFO_RDK_CENTRAL_CONFIGURE_WIFI,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
			Dut clientDevice = BroadBandConnectedClientUtils
					.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			status = clientDevice != null && disableCaptivePortal && disableWifiConfigure;

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfully disabled captive portal and connected to client device");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : verify GET of these parameters using WEBPA if they have default values");
			LOGGER.info(
					"STEP 3: ACTION : Execute command: dmcli eRT getv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4"
							+ "dmcli eRT getv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6"
							+ "dmcli eRT getv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv4"
							+ "dmcli eRT getv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv6");
			LOGGER.info("STEP 3: EXPECTED : Check values are returned without any issues");
			LOGGER.info("**********************************************************************************");

			errorMessage = "Default primary ipv4 dns value is mismatch";
			if (BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4,
					BroadbandPropertyFileHandler.getGlobalDNSIpv4Value())) {
				errorMessage = "Default primary ipv6 dns value is mismatch";
				if (BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6,
						BroadbandPropertyFileHandler.getGlobalDNSIpv6Value())) {
					errorMessage = "Default secondary ipv4 dns value is mismatch";
					String secondaryIpv4 = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV4);
					if (CommonMethods.isNull(secondaryIpv4)) {
						errorMessage = "Default secondary ipv6 dns value is mismatch";
						String secobdaryIpv6 = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV6);
						status = CommonMethods.isNull(secobdaryIpv6);
					}
				}
			}

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : successfully verified default dns values");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Enable XDNS with default settings");
			LOGGER.info(
					"STEP 4: ACTION : Execute command :dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceTag string Test_xdns1"
							+ "dmcli eRT getv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceTag"
							+ "dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS bool true"
							+ "dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
			LOGGER.info("STEP 4: EXPECTED : Check values get set and enabled properly");
			LOGGER.info("**********************************************************************************");

			errorMessage = "Unable to set new tag name to device by this parameter "
					+ BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_XDNS_DEVICE_TAG;
			if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_XDNS_DEVICE_TAG, BroadBandTestConstants.CONSTANT_0,
					BroadBandTraceConstants.XDNS_TAG_NAME)) {
				errorMessage = "Not able to enable XDNS by using this parameter "
						+ BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS;
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS,
						BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			}

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Successfully enabled XDNS with default settings.");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s5";
			errorMessage = "dns override is not present in /etc/resolv.conf";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Check /etc/resolv.conf contains XDNS entries");
			LOGGER.info("STEP 5: ACTION : Execute command: cat /etc/resolv.conf");
			LOGGER.info(
					"STEP 5: EXPECTED : Check values are returned in the below format:dnsoverride 00:00:00:00:00:00 <primary ipv4 address> <primary ipv6 address> <Tag Name>dnsoverride 00:00:00:00:00:00 < secondary ipv4 address> < secondary ipv6 address>  <Tag Name>");
			LOGGER.info("**********************************************************************************");

			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.DNS_OVERRIDE_VALUE
							.replace(BroadBandTestConstants.STRING_REPLACE_IPV4,
									BroadbandPropertyFileHandler.getGlobalDNSIpv4Value())
							.replace(BroadBandTestConstants.STRING_REPLACE_IPV6,
									BroadbandPropertyFileHandler.getGlobalDNSIpv6Value()),
					BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH);
			status = CommonMethods.isNotNull(response);

			if (status) {
				LOGGER.info("STEP 5: ACTUAL : Successfully verified dns override is present in /etc/resolv.conf");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s6";
			errorMessage = "DNS packets are not received in tcp dump";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent  to tcp dump when DNS requests in connected clients in browser.");
			LOGGER.info(
					"STEP 6: ACTION : Execute command: 1)tcpdump -i erouter0 port 53 and access webpage in the browser of connected client");
			LOGGER.info("STEP 6: EXPECTED : Packets shoule be sent ");
			LOGGER.info("**********************************************************************************");

			if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				CommonUtils.downloadFileUsingAutoVault(device, tapEnv, BroadBandCommandConstants.FILE_PATH_TCPDUMP,
						BroadBandCommandConstants.FOLDER_PATH_TMP);
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_PERMISSION_TO_TCPDUMP);
			}
			tapEnv.executeCommandUsingSsh(device, tcpDumpWrite);
			tapEnv.executeCommandOnOneIPClients(clientDevice, BroadBandCommandConstants.CMD_PING_GOOGLE_URL);
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			tapEnv.executeCommandUsingSsh(device,
					BroadBandCommandConstants.CMD_TO_KILL_ANY_PROCESS + BroadBandTestConstants.TCPDUMP);
			response = tapEnv.executeCommandUsingSsh(device, tcpDumpRead);
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = CommonMethods.isNotNull(response);
			tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_REMOVE_DUMMY_FILE);

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Successfully received packets sent from client on tcpdump");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s7";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Set different values other than default for XDNS."
					+ "set valid value for Primary and invalid value for Secondary address using dmcli/WEBPA ");
			LOGGER.info(
					"STEP 7: ACTION : Execute command:dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4 string <IPv4>"
							+ "dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6 string <IPv6>"
							+ "dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv4 string <IPv4>"
							+ "dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv6 string <IPv6>");
			LOGGER.info("STEP 7: EXPECTED : Values should get set for all the parameters.");
			LOGGER.info("**********************************************************************************");

			errorMessage = "Not able to change new ip for "
					+ BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4;
			if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, BroadBandTestConstants.CONSTANT_0,
					BroadbandPropertyFileHandler.getDNSValidIpv4Value())) {
				errorMessage = "Not able to change new ip for "
						+ BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6;
				if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6,
						BroadBandTestConstants.CONSTANT_0, BroadbandPropertyFileHandler.getDNSValidIpv6Value())) {
					errorMessage = "Not able to change new ip for "
							+ BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV4;
					if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV4,
							BroadBandTestConstants.CONSTANT_0,
							BroadbandPropertyFileHandler.getDNSInvalidSecondaryIpv4Value())) {
						errorMessage = "Not able to change new ip for "
								+ BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV6;
						status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV6,
								BroadBandTestConstants.CONSTANT_0,
								BroadbandPropertyFileHandler.getDNSInvalidSecondaryIpv6Value());
					}
				}
			}

			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Successfully changed values for primary and secondary DNS IPv4,IPv6.");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s8";
			errorMessage = "New primary and secondary DNS overrides are not logged in /etc/resolv.conf";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Check /etc/resolv.conf contains XDNS entries ");
			LOGGER.info("STEP 8: ACTION : Execute command:cat /etc/resolv.conf");
			LOGGER.info("STEP 8: EXPECTED : Should have XDNS entries of the values set in scenario 1 and updated");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods
					.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.DNS_OVERRIDE_VALUE
									.replace(BroadBandTestConstants.STRING_REPLACE_IPV4,
											BroadbandPropertyFileHandler.getDNSValidIpv4Value())
									.replace(BroadBandTestConstants.STRING_REPLACE_IPV6,
											BroadbandPropertyFileHandler.getDNSValidIpv6Value()),
							BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH))
					&& CommonMethods
							.isNotNull(
									BroadBandCommonUtils.searchLogFiles(tapEnv, device,
											BroadBandTraceConstants.DNS_OVERRIDE_VALUE
													.replace(BroadBandTestConstants.STRING_REPLACE_IPV4,
															BroadbandPropertyFileHandler
																	.getDNSInvalidSecondaryIpv4Value())
													.replace(BroadBandTestConstants.STRING_REPLACE_IPV6,
															BroadbandPropertyFileHandler
																	.getDNSInvalidSecondaryIpv6Value()),
											BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH));

			if (status) {
				LOGGER.info(
						"STEP 8: ACTUAL : Successfully verified new primary and secondary DNS override present in /etc/resolv.conf");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s9";
			errorMessage = "Packets didin't receive from primary XDNS on tcpdump";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 9: DESCRIPTION : Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent to primary XDNS servers when making DNS requests in connected clients in browser for Scenario1 and check browser is accessible.");
			LOGGER.info("STEP 9: ACTION : Execute command: tcpdump -i erouter0 port 53");
			LOGGER.info("STEP 9: EXPECTED : Packets should be sent from primary DNS");
			LOGGER.info("**********************************************************************************");

			tapEnv.executeCommandUsingSsh(device, tcpDumpWrite);
			tapEnv.executeCommandOnOneIPClients(clientDevice, BroadBandCommandConstants.CMD_PING_GOOGLE_URL);
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			tapEnv.executeCommandUsingSsh(device,
					BroadBandCommandConstants.CMD_TO_KILL_ANY_PROCESS + BroadBandTestConstants.TCPDUMP);
			tapEnv.executeCommandUsingSsh(device, tcpDumpReadAndWrite);
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = CommonUtils.searchLogFiles(tapEnv, device,
					BroadBandCommandConstants.CMD_TO_GET_DNS_PRIMARY_IP_TCPDUMP);
			tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_REMOVE_DUMMY_FILE);
			tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_REMOVE_CAPTURE_FILE);

			if (status) {
				LOGGER.info("STEP 9: ACTUAL : Successfully received a packets from primary XDNS on tcpdump.");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s10";
			errorMessage = "Not able to set different values for primary and secondary DNS IPv4,IPv6";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Set different values other than default for XDNS."
					+ "set invalid value for Primary and valid value for Secondary address using dmcli/WEBPA");
			LOGGER.info(
					"STEP 10: ACTION : Execute command:dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4 string <IPv4>"
							+ "dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6 string <IPv6>"
							+ "dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv4 string <IPv4>"
							+ "dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv6 string <IPv6>");
			LOGGER.info("STEP 10: EXPECTED : Values should get set for all the parameters.");
			LOGGER.info("**********************************************************************************");

			if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, BroadBandTestConstants.CONSTANT_0,
					BroadbandPropertyFileHandler.getDNSInvalidPrimaryIpv4Value())) {
				if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6,
						BroadBandTestConstants.CONSTANT_0,
						BroadbandPropertyFileHandler.getDNSInvalidPrimaryIpv6Value())) {
					if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV4,
							BroadBandTestConstants.CONSTANT_0,
							BroadbandPropertyFileHandler.getDNSValidSecondaryIpv4Value())) {
						status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV6,
								BroadBandTestConstants.CONSTANT_0,
								BroadbandPropertyFileHandler.getDNSValidSecondaryIpv6Value());
					}
				}
			}

			if (status) {
				LOGGER.info("STEP 10: ACTUAL : Successfully changed values for primary and secondary DNS IPv4,IPv6.");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s11";
			errorMessage = "New primary and secondary DNS overrides are not logged in /etc/resolv.conf";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Check /etc/resolv.conf contains XDNS entries");
			LOGGER.info("STEP 11: ACTION : Execute command:cat /etc/resolv.conftcpdump -i erouter0 port 53");
			LOGGER.info("STEP 11: EXPECTED : Should have XDNS entries of the values set in scenario 2 and updated");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods
					.isNotNull(
							BroadBandCommonUtils
									.searchLogFiles(tapEnv, device,
											BroadBandTraceConstants.DNS_OVERRIDE_VALUE
													.replace(BroadBandTestConstants.STRING_REPLACE_IPV4,
															BroadbandPropertyFileHandler
																	.getDNSInvalidPrimaryIpv4Value())
													.replace(BroadBandTestConstants.STRING_REPLACE_IPV6,
															BroadbandPropertyFileHandler
																	.getDNSInvalidPrimaryIpv6Value()),
											BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH))
					&& CommonMethods
							.isNotNull(
									BroadBandCommonUtils.searchLogFiles(tapEnv, device,
											BroadBandTraceConstants.DNS_OVERRIDE_VALUE
													.replace(BroadBandTestConstants.STRING_REPLACE_IPV4,
															BroadbandPropertyFileHandler
																	.getDNSValidSecondaryIpv4Value())
													.replace(BroadBandTestConstants.STRING_REPLACE_IPV6,
															BroadbandPropertyFileHandler
																	.getDNSValidSecondaryIpv6Value()),
											BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH));

			if (status) {
				LOGGER.info(
						"STEP 11: ACTUAL : Successfully verified new primary and secondary DNS override present in /etc/resolv.conf");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s12";
			errorMessage = "Packets didin't receive from secondary XDNS on tcpdump";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 12: DESCRIPTION : Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent to secondary XDNS servers when making DNS requests in connected clients in browser for Scenario2 and check browser is accessible");
			LOGGER.info("STEP 12: ACTION : Execute command: tcpdump -i erouter0 port 53");
			LOGGER.info("STEP 12: EXPECTED : Packets should be sent from secondary DNS");
			LOGGER.info("**********************************************************************************");

			tapEnv.executeCommandUsingSsh(device, tcpDumpWrite);
			tapEnv.executeCommandOnOneIPClients(clientDevice, BroadBandCommandConstants.CMD_PING_GOOGLE_URL);
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			tapEnv.executeCommandUsingSsh(device,
					BroadBandCommandConstants.CMD_TO_KILL_ANY_PROCESS + BroadBandTestConstants.TCPDUMP);
			tapEnv.executeCommandUsingSsh(device, tcpDumpReadAndWrite);
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = CommonUtils.searchLogFiles(tapEnv, device,
					BroadBandCommandConstants.CMD_TO_GET_DNS_SECONDARY_IP_TCPDUMP);
			tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_REMOVE_DUMMY_FILE);
			tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_REMOVE_CAPTURE_FILE);

			if (status) {
				LOGGER.info("STEP 12: ACTUAL : Successfully received a packets from secondary XDNS on tcpdump.");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s13";
			errorMessage = "Not able to set different values for primary and secondary DNS IPv4,IPv6";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : Set different values other than default for XDNS."
					+ "set invalid value for Primary and invalid value for Secondary address using dmcli/WEBPA");
			LOGGER.info(
					"STEP 13: ACTION : Execute command:dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4 string <IPv4>"
							+ "dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6 string <IPv6>"
							+ "dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv4 string <IPv4>"
							+ "dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv6 string <IPv6>");
			LOGGER.info("STEP 13: EXPECTED : Values should get set for all the parameters.");
			LOGGER.info("**********************************************************************************");

			if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV4,
					BroadBandTestConstants.CONSTANT_0,
					BroadbandPropertyFileHandler.getDNSAnotherInvalidSecondaryIpv4Value())) {
				status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV6,
						BroadBandTestConstants.CONSTANT_0,
						BroadbandPropertyFileHandler.getDNSAnotherInvalidSecondaryIpv6Value());
			}

			if (status) {
				LOGGER.info("STEP 13: ACTUAL : Successfully changed values for primary and secondary DNS IPv4,IPv6.");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s14";
			errorMessage = "New primary and secondary DNS overrides are not logged in /etc/resolv.conf";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 14: DESCRIPTION : Check /etc/resolv.conf contains XDNS entries ");
			LOGGER.info("STEP 14: ACTION : Execute command:cat /etc/resolv.conftcpdump -i erouter0 port 53");
			LOGGER.info("STEP 14: EXPECTED : Should have XDNS entries of the values set in scenario 3 and updated");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods
					.isNotNull(
							BroadBandCommonUtils
									.searchLogFiles(tapEnv, device,
											BroadBandTraceConstants.DNS_OVERRIDE_VALUE
													.replace(BroadBandTestConstants.STRING_REPLACE_IPV4,
															BroadbandPropertyFileHandler
																	.getDNSInvalidPrimaryIpv4Value())
													.replace(BroadBandTestConstants.STRING_REPLACE_IPV6,
															BroadbandPropertyFileHandler
																	.getDNSInvalidPrimaryIpv6Value()),
											BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH))
					&& CommonMethods
							.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
									BroadBandTraceConstants.DNS_OVERRIDE_VALUE
											.replace(BroadBandTestConstants.STRING_REPLACE_IPV4,
													BroadbandPropertyFileHandler
															.getDNSAnotherInvalidSecondaryIpv4Value())
											.replace(BroadBandTestConstants.STRING_REPLACE_IPV6,
													BroadbandPropertyFileHandler
															.getDNSAnotherInvalidSecondaryIpv6Value()),
									BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH));

			if (status) {
				LOGGER.info(
						"STEP 14: ACTUAL : Successfully verified new primary and secondary DNS override present in /etc/resolv.conf");
			} else {
				LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s15";
			errorMessage = "Browser is accessible on client device";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 15: DESCRIPTION : Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent to tcp dump when making DNS requests in connected clients in browser for Scenario3 and check browser is not accessible");
			LOGGER.info("STEP 15: ACTION : Execute command: tcpdump -i erouter0 port 53");
			LOGGER.info("STEP 15: EXPECTED : Browser should not be accessible");
			LOGGER.info("**********************************************************************************");

			tapEnv.executeCommandUsingSsh(device, tcpDumpWrite);
			tapEnv.executeCommandOnOneIPClients(clientDevice, BroadBandCommandConstants.CMD_PING_GOOGLE_URL);
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			tapEnv.executeCommandUsingSsh(device,
					BroadBandCommandConstants.CMD_TO_KILL_ANY_PROCESS + BroadBandTestConstants.TCPDUMP);
			response = tapEnv.executeCommandUsingSsh(device, tcpDumpRead);
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			BroadBandResultObject internetAccessStatus = null;
			internetAccessStatus = BroadBandConnectedClientUtils.checkInternetConnectivityUsingCurlOrPingforIpv4OrIpv6(
					tapEnv, clientDevice, BroadBandTestConstants.IP_VERSION6);
			status = CommonMethods.isNotNull(response) && !internetAccessStatus.isStatus();
			tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_REMOVE_DUMMY_FILE);

			if (status) {
				LOGGER.info(
						"STEP 15: ACTUAL : Successfully verified browser is not accessible and packets are received in tcpdump.");
			} else {
				LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s16";
			errorMessage = "Not able to set secondary IPv4 and IPv6 to null";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 16: DESCRIPTION : Set Secondary IPv4 and IPv6 address to NULL ");
			LOGGER.info(
					"STEP 16: ACTION : Execute command:dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv4 string \"\""
							+ "dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv6 string \"\"");
			LOGGER.info("STEP 16: EXPECTED : Values should get set and only primary address should be available");
			LOGGER.info("**********************************************************************************");

			if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV4,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.DMCLI_NULL_VALUE)) {
				status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV6,
						BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.DMCLI_NULL_VALUE);
			}

			if (status) {
				LOGGER.info("STEP 16: ACTUAL : Successfully set secondary IPv4 and IPv6 to null");
			} else {
				LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s17";
			errorMessage = "primary DNS override is not logged in /etc/resolv.conf";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 17: DESCRIPTION : verify only primary address in /etc/resolv.conf contains XDNS entries ");
			LOGGER.info("STEP 17: ACTION : Execute command:cat /etc/resolv.conf");
			LOGGER.info("STEP 17: EXPECTED : Values should get set and only primary address should be available");
			LOGGER.info("**********************************************************************************");

			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.DNS_OVERRIDE_VALUE
							.replace(BroadBandTestConstants.STRING_REPLACE_IPV4,
									BroadbandPropertyFileHandler.getDNSInvalidPrimaryIpv4Value())
							.replace(BroadBandTestConstants.STRING_REPLACE_IPV6,
									BroadbandPropertyFileHandler.getDNSInvalidPrimaryIpv6Value()),
					BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH);
			status = CommonMethods.isNotNull(response);

			if (status) {
				LOGGER.info("STEP 17: ACTUAL : successfully verified primary address is present in /etc/resolv.conf");
			} else {
				LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s18";
			errorMessage = "Packets didin't receive from primary XDNS on tcpdump";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 18: DESCRIPTION :Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent to  primary XDNS servers when making DNS requests in connected clients in browser and check browser is accessible");
			LOGGER.info("STEP 18: ACTION : Execute command: tcpdump -i erouter0 port 53");
			LOGGER.info("STEP 18: EXPECTED : DNS request should sent from primary address");
			LOGGER.info("**********************************************************************************");

			tapEnv.executeCommandUsingSsh(device, tcpDumpWrite);
			tapEnv.executeCommandOnOneIPClients(clientDevice, BroadBandCommandConstants.CMD_PING_GOOGLE_URL);
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			tapEnv.executeCommandUsingSsh(device,
					BroadBandCommandConstants.CMD_TO_KILL_ANY_PROCESS + BroadBandTestConstants.TCPDUMP);
			tapEnv.executeCommandUsingSsh(device, tcpDumpReadAndWrite);
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = CommonUtils.searchLogFiles(tapEnv, device,
					BroadBandCommandConstants.CMD_TO_GET_INVALID_DNS_PRIMARY_IP_TCPDUMP);
			tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_REMOVE_DUMMY_FILE);
			tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_REMOVE_CAPTURE_FILE);

			if (status) {
				LOGGER.info("STEP 18: ACTUAL : Successfully received a packets from primary XDNS on tcpdump.");
			} else {
				LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s19";
			errorMessage = "Unable to disable XDNS";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 19: DESCRIPTION : Disable XDNS");
			LOGGER.info(
					"STEP 19: ACTION : Execute command:dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS bool false");
			LOGGER.info("STEP 19: EXPECTED : DNSoverride entries should not be available");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);

			if (status) {
				LOGGER.info("STEP 19: ACTUAL : Successfully disabled ");
			} else {
				LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s20";
			errorMessage = "resolv.conf having dns override entries";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 20: DESCRIPTION : verify resolv.conf does not contain any dnsoverride entries /etc/resolv.conf ");
			LOGGER.info("STEP 20: ACTION : Execute command:cat /etc/resolv.conf");
			LOGGER.info("STEP 20: EXPECTED : Values should get set and only primary address should be available");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.XDNS_TAG_NAME, BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH));

			if (status) {
				LOGGER.info("STEP 20: ACTUAL : Successfully verified resolv.conf not having dns override entries");
			} else {
				LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s21";
			errorMessage = "DNS packets are not received on tcpdump";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 21: DESCRIPTION : Run a tcp dump on erouter0 for DNS requests, verify that DNS packets are sent to both primary and secondary XDNS servers when making DNS requests in connected clients in browser for Scenario1 and check browser is accessible");
			LOGGER.info("STEP 21: ACTION : Execute command: tcpdump -i erouter0 port 53");
			LOGGER.info("STEP 21: EXPECTED : Should have XDNS entries of the values set in scenario 1 and updated");
			LOGGER.info("**********************************************************************************");

			tapEnv.executeCommandUsingSsh(device, tcpDumpWrite);
			tapEnv.executeCommandOnOneIPClients(clientDevice, BroadBandCommandConstants.CMD_PING_GOOGLE_URL);
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			tapEnv.executeCommandUsingSsh(device,
					BroadBandCommandConstants.CMD_TO_KILL_ANY_PROCESS + BroadBandTestConstants.TCPDUMP);
			response = tapEnv.executeCommandUsingSsh(device, tcpDumpRead);
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonMethods.isNotNull(response);
			tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_REMOVE_DUMMY_FILE);

			if (status) {
				LOGGER.info("STEP 21: ACTUAL : Successfully verified dns packets are received on tcpdump.");
			} else {
				LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s22";
			errorMessage = "XDNS_SetParamStringValue is not occur in log files";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 22: DESCRIPTION : Check for \"XDNS_SetParamStringValue\" log after setting secondary values in log files");
			LOGGER.info(
					"STEP 22: ACTION : Execute command:dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv4 string 13.13.12.11dmcli eRT setv Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv6 string 2621:104:a00b::6Check value:grep -i \"XDNS_SetParamStringValue\" /rdklogs/logs/*");
			LOGGER.info("STEP 22: EXPECTED : Log string should be available");
			LOGGER.info("**********************************************************************************");

			if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV4,
					BroadBandTestConstants.CONSTANT_0,
					BroadbandPropertyFileHandler.getDNSAnotherInvalidSecondaryIpv4Value())) {
				if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV6,
						BroadBandTestConstants.CONSTANT_0,
						BroadbandPropertyFileHandler.getDNSAnotherInvalidSecondaryIpv6Value())) {
					status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.LOG_MSG_XDNS_PARAMETER_VALUE,
							BroadBandCommandConstants.DIRECTORY_LOGS + BroadBandTestConstants.ASTERISK));
				}
			}

			if (status) {
				LOGGER.info("STEP 22: ACTUAL : Successfully verified setparamstringvalue is present in log files.");
			} else {
				LOGGER.error("STEP 22: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s23";
			errorMessage = "Enabling secondary XDNS log is not available in /rdklogs/logs/PAMlog.txt.0";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 23: DESCRIPTION : EnableXDNS and check for log string \"Enabling secondary XDNS\"");
			LOGGER.info(
					"STEP 23: ACTION : Execute command:dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS bool true"
							+ "grep -i \"Enabling secondary XDNS\" /rdklogs/logs/PAMlog.txt.0");
			LOGGER.info("STEP 23: EXPECTED : Log string should be available");
			LOGGER.info("**********************************************************************************");

			if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE)) {
				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MSG_SECONDARY_XDNS_ENABLE,
						BroadBandTestConstants.COMMAND_NTP_LOG_FILE));
			}

			if (status) {
				LOGGER.info(
						"STEP 23: ACTUAL : Successfully verified Enabling secondary XDNS log in /rdklogs/logs/PAMlog.txt.0");
			} else {
				LOGGER.error("STEP 23: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info(
					"POST-CONDITION : DESCRIPTION : Disable XDNS and set primary and secondary dns IPv4,IPv6 values to default");
			LOGGER.info(
					"POST-CONDITION : ACTION : Execute command:dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS bool false");
			LOGGER.info("POST-CONDITION : EXPECTED : XDNS should be disabled");

			boolean disableXDNS = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);
			LOGGER.info("Is " + BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS + " changed to false "
					+ disableXDNS);
			boolean primaryIpv4 = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, BroadBandTestConstants.CONSTANT_0,
					BroadbandPropertyFileHandler.getGlobalDNSIpv4Value());
			LOGGER.info("Is Primary XDNS ipv4 changed to default value " + primaryIpv4);
			boolean primaryIpv6 = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6, BroadBandTestConstants.CONSTANT_0,
					BroadbandPropertyFileHandler.getGlobalDNSIpv6Value());
			LOGGER.info("Is Primary XDNS ipv6 changed to default value " + primaryIpv6);
			boolean secondaryIpv4 = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV4,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.DMCLI_NULL_VALUE);
			LOGGER.info("Is Secondary XDNS ipv4 changed to default value " + secondaryIpv4);
			boolean secondaryIpv6 = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV6,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.DMCLI_NULL_VALUE);
			LOGGER.info("Is Secondary XDNS ipv4 changed to default value " + secondaryIpv6);
			status = primaryIpv4 & primaryIpv6 & secondaryIpv4 & secondaryIpv6 & disableXDNS;

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-XDNS-1006");
	}

	/**
	 * Validation of XDNS redirection when clients are excluded from security edge
	 * <ol>
	 * <li>PRE-CONDITION 1 : RETRIEVE ETHERNET CONNECTED CLIENT FROM CONNECTED
	 * CLIENTS LIST</li>
	 * <li>PRE-CONDITION 2 : VERIFY INTERNET CONNECTIVITY IN ETHERNET CONNECTED
	 * CLIENT</li>
	 * <li>PRE-CONDITION 3 : RETRIEVE WIFI CONNECTED CLIENT FROM CONNECTED CLIENTS
	 * LIST</li>
	 * <li>PRE-CONDITION 4 : ENABLE XDNS IF NOT ENABLED</li>
	 * <li>PRE-CONDITION 5 : VERIFY XDNS PROCESS IS RUNNING IN CPE</li>
	 * <li>1. Verify setting primary and secondary ipv4 and ipv6 XDNS servers</li>
	 * <li>2. Verify XDNS server values in /etc/resolv.conf</li>
	 * <li>3. Start tcpdump as background process for packet capture</li>
	 * <li>4. Run ping command in ethernet client</li>
	 * <li>5. Validate DNS packets being routed to akamai server</li>
	 * <li>6. Run ping command in wifi client</li>
	 * <li>7. Valdiate DNS packets being routed to akamai server</li>
	 * <li>8. Set Akakami server exclusion list for wifi client</li>
	 * <li>9. Verify XDNS server values in /etc/resolv.conf</li>
	 * <li>10. Run ping command in ethernet client</li>
	 * <li>11. Valdiate DNS packets being routed to akamai server</li>
	 * <li>12. Run ping command in ethernet client</li>
	 * <li>13. Valdiate DNS packets are not being routed to akamai server</li>
	 * <li>POST-CONDITION 1 : Remove added XDNS table</li>
	 * <li>POST-CONDITION 2 : Set default comcat DNS server values</li>
	 * <li>POST-CONDITION 3 : Set XDNS to false if disabled before execution</li>
	 * <li>POST-CONDITION 4 : Kill tcpdump process running as background
	 * process</li>
	 * <li>POST-CONDITION 5 : Remove akamai.pcap from tmp folder</li>
	 * </ol>
	 * 
	 * @param Dut
	 * 
	 * @author RamaTeja Meduri
	 * @refactor Rakesh C N
	 *
	 */

	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-XDNS_SECURITYEDGE-1001")
	public void testXdnsSecurityEdgeExcluion(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-XDNS_SECURITYEDGE-101";
		String stepNum = "s1";
		String errorMessage = null;
		boolean status = false;
		Dut ethernetClient = null;// Dut object to store ethernetClient
		Dut wifiClient = null;// Dut object to store wifiClient
		String response = null;
		String xdnsEnable = null;
		String xdnsTableCount = null;
		String deviceMac = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-XDNS_SECURITYEDGE-1001");
		LOGGER.info("TEST DESCRIPTION: Verify DNSSec flag when XDNS is enabled");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1 : RETRIEVE ETHERNET CONNECTED CLIENT FROM CONNECTED CLIENTS LIST");
		LOGGER.info("PRE-CONDITION 2 : VERIFY INTERNET CONNECTIVITY IN ETHERNET CONNECTED CLIENT");
		LOGGER.info("PRE-CONDITION 3 : RETRIEVE WIFI CONNECTED CLIENT FROM CONNECTED CLIENTS LIST");
		LOGGER.info("PRE-CONDITION 4 : ENABLE XDNS IF NOT ENABLED");
		LOGGER.info("PRE-CONDITION 5 : VERIFY XDNS PROCESS IS RUNNING IN CPE");
		LOGGER.info("1. Verify setting primary and secondary ipv4 and ipv6 XDNS servers");
		LOGGER.info("2. Verify XDNS server values in /etc/resolv.conf");
		LOGGER.info("3. Start tcpdump as background process for packet capture");
		LOGGER.info("4. Run ping command in ethernet client");
		LOGGER.info("5. Valdiate DNS packets being routed to akamai server");
		LOGGER.info("6. Run ping command in wifi client");
		LOGGER.info("7. Valdiate DNS packets being routed to akamai server");
		LOGGER.info("8. Set Akakami server exclusion list for wifi client");
		LOGGER.info("9. Verify XDNS server values in /etc/resolv.conf");
		LOGGER.info("10. Run ping command in ethernet client");
		LOGGER.info("11. Valdiate DNS packets being routed to akamai server");
		LOGGER.info("12. Run ping command in ethernet client");
		LOGGER.info("13. Valdiate DNS packets are not being routed to akamai server");
		LOGGER.info("POST-CONDITION 1 : Remove added XDNS table");
		LOGGER.info("POST-CONDITION 2 : Set default comcat DNS server values");
		LOGGER.info("POST-CONDITION 3 : Set XDNS to false if disabled before execution");
		LOGGER.info("POST-CONDITION 4 : Kill tcpdump process running as background process");
		LOGGER.info("POST-CONDITION 5 : Remove akamai.pcap from tmp folder");

		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			LOGGER.info(
					"PRE-CONDITION 1 : DESCRIPTION : RETRIEVE ETHERNET CONNECTED CLIENT FROM CONNECTED CLIENTS LIST");
			LOGGER.info("PRE-CONDITION 1 : ACTION : ETHERNET CLIENT SHOULD BE RETRIEVED");
			LOGGER.info("PRE-CONDITION 1 : EXPECTED: ETHERNET CONNECTED CLIENT IS RETRIEVED SUCCESSFULLY");
			errorMessage = "Unable to retrieve ethernet client ";
			try {
				ethernetClient = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = ethernetClient != null;
			if (status) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : Ethernet connected client is retrieved successfully");
			} else {
				LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
				throw new TestException(errorMessage);
			}

			LOGGER.info("**********************************************************************************");

			LOGGER.info("PRE-CONDITION 2 : DESCRIPTION :VERIFY INTERNET CONNECTIVITY IN ETHERNET CONNECTED CLIENT ");
			LOGGER.info(
					"PRE-CONDITION 2 : ACTION : EXECUTE curl --connect-timeout 20 --head -4 google.com SHOULD BE SUCCESSFUL");
			LOGGER.info("PRE-CONDITION 2 : EXPECTED: CONNECTIVITY CHECK SHOULD RETURN STATUS AS 200");
			errorMessage = "No Internet connectivity for Ethernet connected client ";
			BroadBandResultObject broadBandResultObject = BroadBandConnectedClientUtils
					.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv, ethernetClient,
							BroadBandTestConstants.URL_GOOGLE, BroadBandTestConstants.IP_VERSION4);
			status = broadBandResultObject.isStatus();
			errorMessage = broadBandResultObject.getErrorMessage();

			if (status) {
				LOGGER.info("PRE-CONDITION 2 : ACTUAL : Internet connectivity successful using ipv4 with Curl request");
			} else {
				LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
				throw new TestException(errorMessage);
			}
			LOGGER.info("**********************************************************************************");

			LOGGER.info("PRE-CONDITION 3 : DESCRIPTION : RETRIEVE WIFI CONNECTED CLIENT FROM CONNECTED CLIENTS LIST");
			LOGGER.info("PRE-CONDITION 3 : ACTION : WIFI CLIENT SHOULD BE RETRIEVED");
			LOGGER.info("PRE-CONDITION 3 : EXPECTED: WIFI CONNECTED CLIENT IS RETRIEVED SUCCESSFULLY");
			errorMessage = "Unable to retrieve wifi client ";
			try {
				wifiClient = BroadBandConnectedClientUtils
						.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = wifiClient != null;
			if (status) {
				LOGGER.info("PRE-CONDITION 3 : ACTUAL : Wifi connected client is retrieved successfully");
			} else {
				LOGGER.error("PRE-CONDITION 3 : ACTUAL : " + errorMessage);
				throw new TestException(errorMessage);
			}
			LOGGER.info("**********************************************************************************");

			LOGGER.info("PRE-CONDITION 4 : DESCRIPTION :ENABLE XDNS IF NOT ENABLED ");
			LOGGER.info("PRE-CONDITION 4 : ACTION : tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS bool true");
			LOGGER.info("PRE-CONDITION 4 : EXPECTED: XDNS MUST Be enabled");
			errorMessage = "XDNS is not enabled";
			xdnsEnable = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS);
			if (xdnsEnable.equalsIgnoreCase(BroadBandTestConstants.FALSE)) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS,
						WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
			}

			if (status) {
				LOGGER.info("PRE-CONDITION 4 : ACTUAL : Internet connectivity successful using ipv4 with Curl request");
			} else {
				LOGGER.error("PRE-CONDITION 4 : ACTUAL : " + errorMessage);
				throw new TestException(errorMessage);
			}
			LOGGER.info("**********************************************************************************");

			LOGGER.info("PRE-CONDITION 5 : DESCRIPTION :VERIFY XDNS PROCESS IS RUNNING IN CPE ");
			LOGGER.info("PRE-CONDITION 5 : ACTION : pidof CcspXdnsSsp");
			LOGGER.info("PRE-CONDITION 5 : EXPECTED: XDNS PROCESS MUST BE RUNNING IN CPE");
			errorMessage = "XDNS process is not running CPE";
			status = CommonMethods.isNotNull(
					CommonMethods.getPidOfProcess(device, tapEnv, BroadBandCommandConstants.POROCESS_NAME_CCSPXDNSSSP));

			if (status) {
				LOGGER.info("PRE-CONDITION 5 : ACTUAL : Internet connectivity successful using ipv4 with Curl request");
			} else {
				LOGGER.error("PRE-CONDITION 5 : ACTUAL : " + errorMessage);
				throw new TestException(errorMessage);
			}
			LOGGER.info("**********************************************************************************");

			stepNum = "s1";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify setting primary and secondary ipv4 and ipv6 XDNS servers");
			LOGGER.info(
					"STEP 1: ACTION : Execute webpa or dmcli command for following sets:\\nDevice.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4 string <ip>\\nDevice.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6 string <ip>\\nDevice.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv4 string <ip>\\nDevice.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv6 string <ip>\\nDevice.X_RDKCENTRAL-COM_XDNS.DefaultDeviceTag string SecurityEdge\\nVerify above values with get after performing set operation.");
			LOGGER.info("STEP 1: EXPECTED : Successfully set ipv4 and ipv6 XDNS servers");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to set security edge configurations";
			if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, BroadBandTestConstants.CONSTANT_0,
					BroadBandTestConstants.VALUE_PRIMARY_IPV4_XDNS)) {
				errorMessage = "Failed to set value of Primary IPv6 Dns parameter";
				if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6,
						BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.VALUE_PRIMARY_IPV6_XDNS)) {
					errorMessage = "Failed to set value of secondary IPv4 Dns parameter";
					if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV4,
							BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.VALUE_SECONDARY_IPV4_XDNS)) {
						errorMessage = "Failed to set value of secondary IPv6 Dns parameter";
						if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV6,
								BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.VALUE_SECONDARY_IPV6_XDNS)) {
							errorMessage = "Failed to set value of device tag for XDNS";
							status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
									BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_XDNS_DEVICE_TAG,
									BroadBandTestConstants.CONSTANT_0,
									BroadBandTestConstants.STRING_XDNS_SECURITY_EDGE);
						}
					}
				}
			}

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully set ipv4 and ipv6 XDNS servers");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify XDNS server values in /etc/resolv.conf");
			LOGGER.info("STEP 2: ACTION : Execute command:cat /etc/resolv.conf");
			LOGGER.info("STEP 2: EXPECTED : Resolv.conf has updated XDNS values");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to get content of /etc/resolv.conf";
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTestConstants.CAT_COMMAND, BroadBandTestConstants.RESOLVE_DOT_CONF_FILE));
			if (CommonMethods.isNotNull(response)) {
				errorMessage = "Primary IPv4 XDNS value not updated in resolv.conf file";
				if (CommonMethods.patternMatcher(response, BroadBandTestConstants.VALUE_PRIMARY_IPV4_XDNS)) {
					errorMessage = "Primary IPv6 XDNS value not updated in resolv.conf file";
					if (CommonMethods.patternMatcher(response, BroadBandTestConstants.VALUE_PRIMARY_IPV6_XDNS)) {
						errorMessage = "Secondary IPv4 XDNS value not updated in resolv.conf file";
						if (CommonMethods.patternMatcher(response, BroadBandTestConstants.VALUE_SECONDARY_IPV4_XDNS)) {
							errorMessage = "Secondary IPv6 XDNS value not updated in resolv.conf file";
							if (CommonMethods.patternMatcher(response,
									BroadBandTestConstants.VALUE_SECONDARY_IPV6_XDNS)) {
								errorMessage = "Security Edge tag value not updated in resolv.conf file";
								status = CommonMethods.patternMatcher(response,
										BroadBandTestConstants.STRING_XDNS_SECURITY_EDGE);
							}
						}
					}
				}
			}

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Resolv.conf has updated XDNS values");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Start tcpdump as background process for packet capture");
			LOGGER.info("STEP 3: ACTION : Execute command:/usr/sbin/tcpdump -i erouter0 > /tmp/akamai.pcap 2>&1 &");
			LOGGER.info("STEP 3: EXPECTED : tcpdump must be running as background process to capture packets");
			LOGGER.info("**********************************************************************************");
			errorMessage = "tcpdump is not running as background process to capture packets";
			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TCPDUMP_EROUTER0_AKAMAI);
			status = CommonMethods
					.isNotNull(CommonMethods.getPidOfProcess(device, tapEnv, BroadBandCommandConstants.CMD_TCPDUMP));

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : tcpdump is running running and capturing packets to /tmp/akamai.pcap");
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Run ping command in ethernet client");
			LOGGER.info("STEP 4: ACTION : Execute command on ethernet client:ping eenadu.net -c 20");
			LOGGER.info("STEP 4: EXPECTED : ping must be successful");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to execute ping command on etherent client";
			status = CommonMethods.isNotNull(
					tapEnv.executeCommandOnOneIPClients(ethernetClient, BroadBandCommandConstants.CMD_ETH_PING_20));
			if (status) {
				LOGGER.info("STEP 4: ACTUAL : ping is successful on ethernet client");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s5";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Valdiate DNS packets being routed to akamai server");
			LOGGER.info(
					"STEP 5: ACTION : cat /tmp/akakami.pcap|grep -I eenadu.net|grep -I <IP>|grep -I <IP>");
			LOGGER.info("STEP 5: EXPECTED : DNS packets must be routed to akamai server");
			LOGGER.info("**********************************************************************************");
			errorMessage = "DNS packets are not routed to akamai server";
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTestConstants.STRING_XDNS_BROWSE,
					BroadBandCommandConstants.PATH_AKAMAI_CAPTURE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.VALUE_PRIMARY_IPV4_XDNS)
					|| CommonUtils.patternSearchFromTargetString(response,
							BroadBandTestConstants.VALUE_SECONDARY_IPV4_XDNS);
			if (!status) {
				status = CommonUtils.patternSearchFromTargetString(response,
						BroadBandTestConstants.VALUE_PRIMARY_IPV6_XDNS)
						|| CommonUtils.patternSearchFromTargetString(response,
								BroadBandTestConstants.VALUE_SECONDARY_IPV6_XDNS);
			}
			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_EMPTY_PACKET_AKAMAI);
			if (status) {
				LOGGER.info("STEP 5: ACTUAL : Successfully disabled XDNS feature");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			LOGGER.info("STEP 6: DESCRIPTION : Run ping command in wifi client");
			LOGGER.info("STEP 6: ACTION : Execute command on wifi client:ping -n 20 eenadu.net");
			LOGGER.info("STEP 6: EXPECTED : ping must be successful");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to execute ping command on wifi client";
			status = CommonMethods.isNotNull(
					tapEnv.executeCommandOnOneIPClients(wifiClient, BroadBandCommandConstants.CMD_WIFI_PING_20));
			if (status) {
				LOGGER.info("STEP 6: ACTUAL : ping is successful on wifi client");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s7";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Valdiate DNS packets being routed to akamai server");
			LOGGER.info("STEP 7: ACTION : cat /tmp/akakami.pcap|grep -I eenadu.net");
			LOGGER.info("STEP 7: EXPECTED : DNS packets must be routed to akamai server");
			LOGGER.info("**********************************************************************************");
			errorMessage = "DNS packets are not routed to akamai server";
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTestConstants.STRING_XDNS_BROWSE,
					BroadBandCommandConstants.PATH_AKAMAI_CAPTURE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.VALUE_PRIMARY_IPV4_XDNS)
					|| CommonUtils.patternSearchFromTargetString(response,
							BroadBandTestConstants.VALUE_SECONDARY_IPV4_XDNS);
			if (!status) {
				status = CommonUtils.patternSearchFromTargetString(response,
						BroadBandTestConstants.VALUE_PRIMARY_IPV6_XDNS)
						|| CommonUtils.patternSearchFromTargetString(response,
								BroadBandTestConstants.VALUE_SECONDARY_IPV6_XDNS);
			}
			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_EMPTY_PACKET_AKAMAI);
			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Successfully disabled XDNS feature");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s8";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Set Akakami server exclusion list for wifi client");
			LOGGER.info(
					"STEP 8: ACTION : dmcli eRT addtable Device.X_RDKCENTRAL-COM_XDNS.DNSMappingTable.\ntr181.Device.X_RDKCENTRAL-COM_XDNS.DNSMappingTable.1.MacAddress string 00:0e:8e:92:b6:9d\ntr181.Device.X_RDKCENTRAL-COM_XDNS.DNSMappingTable.1.DnsIPv4 string 75.75.75.75\ntr181.Device.X_RDKCENTRAL-COM_XDNS.DNSMappingTable.1.DnsIPv6 string 2001:558:feed::1\ntr181.Device.X_RDKCENTRAL-COM_XDNS.DNSMappingTable.1.Tag string SecurityEdge_Exclusion");
			LOGGER.info("STEP 8: EXPECTED : Akamami server exclusion list must be added to wifi client");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Akamami server exclusion list is not added to wifi client";
			deviceMac = BroadBandConnectedClientUtils.getConnectedClientIpOrMacFromTheDevice(device, wifiClient, tapEnv,
					false);
			xdnsTableCount = tapEnv.executeCommandUsingSsh(device,
					BroadBandCommonUtils.concatStringUsingStringBuffer(
							BroadBandCommandConstants.DMCLI_PREFIX_TO_ADD_TABLE, AutomaticsConstants.SPACE,
							BroadBandWebPaConstants.WEBPA_PARAM_DNS_MAPPING_TABLE,
							BroadBandCommandConstants.CMD_GREP_XDNS_DATA_TABLE));
			xdnsTableCount = xdnsTableCount.replace(BroadBandTestConstants.DELIMITER_NEW_LINE,
					BroadBandTestConstants.EMPTY_STRING);

			if (CommonMethods.isNotNull(xdnsTableCount)) {
				if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_XDNS_MAC_ADDR_MAPPING_TABLE
								.replace(BroadBandTestConstants.TR181_NODE_REF, xdnsTableCount),
						BroadBandTestConstants.CONSTANT_0, deviceMac.toLowerCase())) {
					if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_XDNS_IPV4_MAPPING_TABLE
									.replace(BroadBandTestConstants.TR181_NODE_REF, xdnsTableCount),
							BroadBandTestConstants.CONSTANT_0,
							BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE)) {
						if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_XDNS_IPV6_MAPPING_TABLE
										.replace(BroadBandTestConstants.TR181_NODE_REF, xdnsTableCount),
								BroadBandTestConstants.CONSTANT_0,
								BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV6_VALUE)) {
							status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
									BroadBandWebPaConstants.WEBPA_PARAM_XDNS_CLIENT_TAG_NAME_MAPPING_TABLE
											.replace(BroadBandTestConstants.TR181_NODE_REF, xdnsTableCount),
									BroadBandTestConstants.CONSTANT_0,
									BroadBandTestConstants.STRING_XDNS_SECURITY_EDGE_EXCLUSION);
						}
					}
				}
			}

			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Successfully set security edge exclusion parameters");
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s9";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify XDNS server values in /etc/resolv.conf");
			LOGGER.info("STEP 9: ACTION : Execute command:cat /etc/resolv.conf");
			LOGGER.info("STEP 9: EXPECTED : Resolv.conf has updated XDNS values");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to get content of /etc/resolv.conf";
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTestConstants.CAT_COMMAND, BroadBandTestConstants.RESOLVE_DOT_CONF_FILE));
			if (CommonMethods.isNotNull(response)) {
				errorMessage = "Primary IPv4 XDNS value not updated in resolv.conf file";
				if (CommonMethods.patternMatcher(response, BroadBandTestConstants.VALUE_PRIMARY_IPV4_XDNS)) {
					errorMessage = "Primary IPv6 XDNS value not updated in resolv.conf file";
					if (CommonMethods.patternMatcher(response, BroadBandTestConstants.VALUE_PRIMARY_IPV6_XDNS)) {
						errorMessage = "Secondary IPv4 XDNS value not updated in resolv.conf file";
						if (CommonMethods.patternMatcher(response, BroadBandTestConstants.VALUE_SECONDARY_IPV4_XDNS)) {
							errorMessage = "Secondary IPv6 XDNS value not updated in resolv.conf file";
							if (CommonMethods.patternMatcher(response,
									BroadBandTestConstants.VALUE_SECONDARY_IPV6_XDNS)) {
								errorMessage = "Security Edge exclusion tag value not updated in resolv.conf file";
								if (CommonMethods.patternMatcher(response,
										BroadBandTestConstants.STRING_XDNS_SECURITY_EDGE_EXCLUSION)) {
									errorMessage = "wifi client mac address value not updated in resolv.conf file";
									if (CommonMethods.patternMatcher(response, deviceMac.toLowerCase())) {
										errorMessage = "Security Edge tag value not updated in resolv.conf file";
										status = CommonMethods.patternMatcher(response,
												BroadBandTestConstants.STRING_XDNS_SECURITY_EDGE);
									}
								}
							}
						}
					}
				}
			}

			if (status) {
				LOGGER.info("STEP 9: ACTUAL : Resolv.conf has updated XDNS values");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s10";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Run ping command in ethernet client");
			LOGGER.info("STEP 10: ACTION : Execute command on ethernet client:ping eenadu.net -c 20");
			LOGGER.info("STEP 10: EXPECTED : ping must be successful");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to execute ping command on etherent client";
			status = CommonMethods.isNotNull(
					tapEnv.executeCommandOnOneIPClients(ethernetClient, BroadBandCommandConstants.CMD_ETH_PING_20));
			if (status) {
				LOGGER.info("STEP 10: ACTUAL : ping is successful on ethernet client");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s11";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Valdiate DNS packets being routed to akamai server");
			LOGGER.info(
					"STEP 11: ACTION : cat /tmp/akakami.pcap|grep -I eenadu.net|grep -I 74.121.125.53|grep -I 74.121.125.54");
			LOGGER.info("STEP 11: EXPECTED : DNS packets must be routed to akamai server");
			LOGGER.info("**********************************************************************************");
			errorMessage = "DNS packets are not routed to akamai server";
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTestConstants.STRING_XDNS_BROWSE,
					BroadBandCommandConstants.PATH_AKAMAI_CAPTURE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.VALUE_PRIMARY_IPV4_XDNS)
					|| CommonUtils.patternSearchFromTargetString(response,
							BroadBandTestConstants.VALUE_SECONDARY_IPV4_XDNS);
			if (!status) {
				status = CommonUtils.patternSearchFromTargetString(response,
						BroadBandTestConstants.VALUE_PRIMARY_IPV6_XDNS)
						|| CommonUtils.patternSearchFromTargetString(response,
								BroadBandTestConstants.VALUE_SECONDARY_IPV6_XDNS);
			}
			tapEnv.executeCommandUsingSsh(device, "echo > /tmp/akamai.pcap");
			if (status) {
				LOGGER.info("STEP 11: ACTUAL : Successfully disabled XDNS feature");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s12";
			status = false;

			LOGGER.info("STEP 12: DESCRIPTION : Run ping command in wifi client");
			LOGGER.info("STEP 12: ACTION : Execute command on ethernet client:ping -n 20 eenadu.net");
			LOGGER.info("STEP 12: EXPECTED : ping must be successful");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to execute ping command on etherent client";
			status = CommonMethods.isNotNull(
					tapEnv.executeCommandOnOneIPClients(wifiClient, BroadBandCommandConstants.CMD_WIFI_PING_20));
			if (status) {
				LOGGER.info("STEP 12: ACTUAL : ping is successful on wifi client");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s13";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : Valdiate DNS packets are not being routed to akamai server");
			LOGGER.info(
					"STEP 13: ACTION : cat /tmp/akakami.pcap|grep -I eenadu.net|grep -I <IP>|grep -I <IP>");
			LOGGER.info("STEP 13: EXPECTED : DNS packets must not be routed to akamai server");
			LOGGER.info("**********************************************************************************");
			errorMessage = "DNS packets are routed to akamai server";
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTestConstants.STRING_XDNS_BROWSE,
					BroadBandCommandConstants.PATH_AKAMAI_CAPTURE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = (!CommonUtils.patternSearchFromTargetString(response,
					BroadBandTestConstants.VALUE_PRIMARY_IPV4_XDNS))
					&& (!CommonUtils.patternSearchFromTargetString(response,
							BroadBandTestConstants.VALUE_SECONDARY_IPV4_XDNS))
					&& (!CommonUtils.patternSearchFromTargetString(response,
							BroadBandTestConstants.VALUE_PRIMARY_IPV6_XDNS))
					&& (!CommonUtils.patternSearchFromTargetString(response,
							BroadBandTestConstants.VALUE_SECONDARY_IPV6_XDNS));

			if (status) {
				LOGGER.info("STEP 13: ACTUAL : Successfully disabled XDNS feature");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Remove added XDNS table");
			LOGGER.info(
					"POST-CONDITION 1 : ACTION : dmcli eRT deltable Device.X_RDKCENTRAL-COM_XDNS.DNSMappingTable.1.");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : DNS Mapping Table must be deleted");
			LOGGER.info("#######################################################################################");
			response = tapEnv.executeCommandUsingSsh(device,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.DMCLI_DEL_TABLE,
							AutomaticsConstants.SPACE, BroadBandWebPaConstants.WEBPA_PARAM_DNS_MAPPING_TABLE,
							xdnsTableCount, BroadBandTestConstants.DOT_OPERATOR));
			status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
					BroadBandTestConstants.RESPONSE_EXECUTION_SUCCEED);
			if (status) {
				LOGGER.info("POST-CONDITION 1 : EXPECTED : DNS Mapping Table must be deleted");
			} else {
				LOGGER.error("POST-CONDITION 1 :  " + errorMessage);
			}

			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 2 : DESCRIPTION : Set default comcat DNS server values");
			LOGGER.info(
					"POST-CONDITION 2 : ACTION : tr181.Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4 string 75.75.75.75\ntr181.Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6 string <IPV6>\ntr181.Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv4 string \"\"\ntr181.Device.X_RDKCENTRAL-COM_XDNS.DefaultSecondaryDeviceDnsIPv6 string \"\"\ntr181.Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceTag string empty");
			LOGGER.info("POST-CONDITION 2 : EXPECTED : XDNS configs must be restored to default");
			LOGGER.info("#######################################################################################");
			BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, BroadBandTestConstants.CONSTANT_0,
					BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE);
			BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6, BroadBandTestConstants.CONSTANT_0,
					BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV6_VALUE);
			DmcliUtils.setWebPaParameterValueUsingDmcliCommand(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV4,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_OPEN_DNS_IP_75_76);
			DmcliUtils.setWebPaParameterValueUsingDmcliCommand(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_SECONDARY_XDNS_IPV6,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.DNS_SERVER_2);
			DmcliUtils.setWebPaParameterValueUsingDmcliCommand(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_XDNS_DEVICE_TAG, BroadBandTestConstants.CONSTANT_0,
					BroadBandTestConstants.EMPTY_KEYWORD.toLowerCase());

			if (status) {
				LOGGER.info("POST-CONDITION 2 : EXPECTED : XDNS configs must be restored to default");
			} else {
				LOGGER.error("POST-CONDITION 2 :  " + errorMessage);
			}

			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 3 : DESCRIPTION : Set XDNS to false if disabled before execution");
			LOGGER.info("POST-CONDITION 3 : ACTION : tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS bool false");
			LOGGER.info("POST-CONDITION 3 : EXPECTED : XDNS must be restored to default");
			LOGGER.info("#######################################################################################");
			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, WebPaDataTypes.BOOLEAN.getValue(),
					xdnsEnable);

			if (status) {
				LOGGER.info("POST-CONDITION 3 : EXPECTED : XDNS must be restored to default");
			} else {
				LOGGER.error("POST-CONDITION 3 :  " + errorMessage);
			}

			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 4 : DESCRIPTION : Kill tcpdump process running as background process");
			LOGGER.info("POST-CONDITION 4 : ACTION : killall -11 tcpdump");
			LOGGER.info("POST-CONDITION 4 : EXPECTED : tcpdump process must be killed");
			LOGGER.info("#######################################################################################");
			status = BroadBandCommonUtils.killAndCheckProcess(device, tapEnv, BroadBandCommandConstants.CMD_TCPDUMP);

			if (status) {
				LOGGER.info("POST-CONDITION 4 : EXPECTED : tcpdump process must be killed");
			} else {
				LOGGER.error("POST-CONDITION 4 :  " + errorMessage);
			}

			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 5 : DESCRIPTION : Remove akamai.pcap from tmp folder");
			LOGGER.info("POST-CONDITION 5 : ACTION : rm /tmp/akamai.pcap");
			LOGGER.info("POST-CONDITION 5 : EXPECTED : /tmp/akamai.pcap must be removed");
			LOGGER.info("#######################################################################################");
			status = BroadBandCommonUtils.removeFileAndVerifyStatus(tapEnv, device,
					BroadBandCommandConstants.PATH_AKAMAI_CAPTURE);

			if (status) {
				LOGGER.info("POST-CONDITION 5 : EXPECTED : /tmp/akamai.pcap must be removed");
			} else {
				LOGGER.error("POST-CONDITION 5 :  " + errorMessage);
			}

			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-XDNS_SECURITYEDGE-1001");
	}
}
