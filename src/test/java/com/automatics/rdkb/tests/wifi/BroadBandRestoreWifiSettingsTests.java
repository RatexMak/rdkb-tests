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
package com.automatics.rdkb.tests.wifi;

import java.util.Map;

import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants.RdkBWifiParameters;
import com.automatics.rdkb.constants.BroadBandWebPaConstants.WIFI_RESTORE_METHOD;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandRestoreWifiUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

/**
 * Class to hold the tests scripts of wifi settings restore scenarios
 * 
 * @author Anandam.S
 * @refactor Govardhan
 */
public class BroadBandRestoreWifiSettingsTests extends AutomaticsTestBase {
	/**
	 * Test to Verify WiFi settings like SSID ,Network Security mode,Channel
	 * Selection Mode,Channel bandwidth,Network name enabled status,Guard interval
	 * ,Network active status,Supported Protocols,Radio Status and Radio Channel can
	 * be restored to default settings using webpa command for doing WIFI settings
	 * reset.
	 * 
	 * <li>Precondition:Set all WIFI parametrs to a non default value using
	 * webpa</li>
	 * <li>STEP 1:Restore WiFi Settings using webpa</li>
	 * <li>STEP 2:Verify the default SSID Name for 2.4 GHz band using WebPA</li>
	 * <li>STEP 3:Verify Wifi network security mode for 2.4 GHz band</li>
	 * <li>STEP 4: Verify Wifi network enabled status for 2.4 GHz Wifi band</li>
	 * <li>STEP 5:Verify Wifi network channel selection mode for 2.4 GHz Wifi
	 * band.</li>
	 * <li>STEP 6:Verify Wifi network channel bandwidth for 2.4 GHz Wifi band</li>
	 * <li>STEP 7: Verify Wifi Broadcast network Name enabled status for 2.4 GHz
	 * Wifi band</li>
	 * <li>STEP 8: Verify Guard interval for 2.4 GHz Wifi network</li>;
	 * <li>STEP 9: Verify WiFi network active status for 2.4 GHz in Connection
	 * Status page</li>
	 * <li>STEP 10: Verify the WiFi supported protocols for 2.4Ghz band</li>
	 * <li>STEP 11: Verify the Radio status for 2.4GHZ band using WebPA</li>
	 * <li>STEP 12: Verify the default SSID Name for 5GHz band using WebPA</li>
	 * <li>STEP 13: Verify Wifi network security mode for 5 GHz band</li>
	 * <li>STEP 14: Verify Wifi network enabled status for 5 GHz Wifi band</li>
	 * <li>STEP 15: Verify Wifi network channel selection mode for 5 GHz Wifi
	 * band</li>
	 * <li>STEP 16:Verify Wifi network channel bandwidth for 5GHz Wifi band</li>
	 * <li>STEP 17: Verify Wifi Broadcast network Name enabled status for 5 GHz Wifi
	 * band</li>
	 * <li>STEP 18: Verify Guard interval for 5 GHz Wifi network</li>
	 * <li>STEP 19: Verify WiFi network active status for 5GHz in Connection Status
	 * page</li>
	 * <li>STEP 20: Verify the WiFi supported protocols for 5 Ghz band</li>
	 * <li>STEP 21: Verify the Radio status for 5GHZ band using WebPA</li>
	 * 
	 * @param device {@link Dut}
	 * @author anandam.s
	 * @refactor Govardhan
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-RESTORE-1001")
	public void testVerifyWifiDefaultSettingsAfterWiFiResetUsingWebpa(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-RESTORE-101";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;

		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: " + testId);
			LOGGER.info(
					"TEST DESCRIPTION: Verify WiFi settings can be restored to default settings using webpa command for setting reset");

			LOGGER.info("TEST STEPS : ");
			LOGGER.info("Precondition:Set the below parametrs to a non  default value using webpa");
			LOGGER.info("1. SSID name");
			LOGGER.info("2. Network Security mode ");
			LOGGER.info("3. Network Enabled status");
			LOGGER.info("4. Channel Selection Mode ");
			LOGGER.info("5. Channel bandwidth");
			LOGGER.info("6. Netwok name enabled sttaus ");
			LOGGER.info("7. Guard interval");
			LOGGER.info("8. Network active status");
			LOGGER.info("9.Supported Protocols");
			LOGGER.info("10.Radio Status");
			LOGGER.info("11.Radio Channels");
			LOGGER.info("STEP 1:Restore WiFi Settings using WEBPA ");
			LOGGER.info("STEP 2:Verify the default SSID Name for 2.4 GHz band using WebPA");
			LOGGER.info("STEP 3:Verify Wifi network security mode for 2.4 GHz band ");
			LOGGER.info("STEP 4: Verify Wifi network enabled status for 2.4 GHz Wifi band");
			LOGGER.info("STEP 5:Verify Wifi network channel selection mode for 2.4 GHz Wifi band.");
			LOGGER.info("STEP 6:Verify Wifi network channel bandwidth for 2.4 GHz Wifi band");
			LOGGER.info("STEP 7: Verify Wifi Broadcast network Name enabled status for 2.4 GHz Wifi band ");
			LOGGER.info("STEP 8: Verify Guard interval for 2.4 GHz Wifi network ");
			LOGGER.info("STEP 9: Verify WiFi network active status for 2.4 GHz in Connection Status page");
			LOGGER.info("STEP 10: Verify the WiFi supported protocols for 2.4Ghz band ");
			LOGGER.info("STEP 11: Verify the Radio status for 2.4GHZ band using WebPA");
			LOGGER.info("STEP 12: Verify the default SSID Name for 5GHz band using WebPA");
			LOGGER.info("STEP 13: Verify Wifi network security mode for 5 GHz band");
			LOGGER.info("STEP 14: Verify Wifi network enabled status for 5 GHz Wifi band");
			LOGGER.info("STEP 15: Verify Wifi network channel selection mode for 5 GHz Wifi band");
			LOGGER.info("STEP 16:Verify Wifi network channel bandwidth for 5GHz Wifi band");
			LOGGER.info("STEP 17: Verify Wifi Broadcast network Name enabled status for 5 GHz Wifi band");
			LOGGER.info("STEP 18: Verify Guard interval for 5 GHz Wifi network");
			LOGGER.info("STEP 19: Verify WiFi network active status for 5GHz in Connection Status page");
			LOGGER.info("STEP 20: Verify the WiFi supported protocols for 5 Ghz band");
			LOGGER.info("STEP 21: Verify the Radio status for 5GHZ band using WebPA");
			LOGGER.info("#######################################################################################");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("PRE-CONDITION : DESCRIPTION::Set the below parametrs to a non  default value using webpa");
			LOGGER.info("1. SSID name");
			LOGGER.info("2. Network Security mode ");
			LOGGER.info("3. Network Enabled status");
			LOGGER.info("4. Channel Selection Mode ");
			LOGGER.info("5. Channel bandwidth");
			LOGGER.info("6. Netwok name enabled sttaus ");
			LOGGER.info("7. Guard interval");
			LOGGER.info("8. Network active status");
			LOGGER.info("9.Supported Protocols");
			LOGGER.info("10.Radio Status");
			LOGGER.info("11.Radio Channels");
			LOGGER.info("PRE-CONDITION : EXPECTED: All the test parameters must be set to non default values");
			try {
				status = BroadBandRestoreWifiUtils.setNonDefaultValuesForAllWifiParameters(device, tapEnv);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION : ACTUAL : PRE CONDITION EXECUTED SUCCESSFULLY");
			} else {
				LOGGER.error("PRE-CONDITION : ACTUAL : " + errorMessage);
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

			testStepNumber = "s1";
			status = false;
			errorMessage = "Restore WIFI settings using webpa failed";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Restore WiFi Settings using WEBPA ");
			LOGGER.info(
					"STEP 1: ACTION      : Set the webpa Device.WiFi.X_CISCO_COM_FactoryResetRadioAndAp as \"1,2:1,2\" ");
			LOGGER.info("STEP 1: EXPECTED    : Webpa should return success");
			LOGGER.info("**********************************************************************************");
			if (!DeviceModeHandler.isRPIDevice(device)) {
				status = BroadBandRestoreWifiUtils.restoreDefaultWifiSettings(device, tapEnv,
						WIFI_RESTORE_METHOD.WEBPA);
			} else {
				status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);
			}
			if (status) {
				LOGGER.info("STEP 1:ACTUAL : Wi-Fi Settings restored successfully");
			} else {
				LOGGER.error("STEP 1:ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			LOGGER.info("Waiting for 6 minutes for changes to get affected");
			tapEnv.waitTill(BroadBandTestConstants.SIX_MINUTE_IN_MILLIS);

			/** Steps 2 to 19 */
			verifyAllWifiParameters(device, testId, new String[] { "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
					"12", "13", "14", "15", "16", "17", "18", "19", "20", "21" }, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception occured during execution !!!!" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-RESTORE-1001");
	}

	/**
	 * Verify all wifi parameters for 2.4 and 5Ghz and update the step status
	 * 
	 * @param device       device under test
	 * @param testId       testid under current execution
	 * @param stepNumbers  stepsnumbers to be updated
	 * @param isSnmpNeeded true if snmp verification is preferred
	 */
	public void verifyAllWifiParameters(Dut device, String testId, String[] stepNumbers, boolean isSnmpNeeded) {
		String stepNumber = null;
		boolean status = false;
		String errorMessage = null;
		boolean isFibreOrDSLDevice = false;

		if (DeviceModeHandler.isFibreDevice(device) || DeviceModeHandler.isDSLDevice(device)) {
			isFibreOrDSLDevice = true;
			LOGGER.info("Is Fibre Device or DSL Device: " + isFibreOrDSLDevice);
		}
		// Get All the webpa params for wifi settings 2.4GHZ
		Map<String, String> wifiParameterMap = BroadBandRestoreWifiUtils.getAllWifiParametersForRadioUsingWebpa(device,
				tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);

		stepNumber = stepNumbers[0];
		status = false;
		errorMessage = "The default security mode and encryption  are not WPA2-Personal and AES";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION:Verify Wifi network security mode for 2.4 GHz band ");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION     :Execute Device.WiFi.AccessPoint.10001.Security.ModeEnabled and Device.WiFi.AccessPoint.10001.Security.X_CISCO_COM_EncryptionMethod ");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED   : The security mode should be 'WPA2-Personal' and the Encryption method should be \"AES\" ");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					RdkBWifiParameters.SECURITY_MODE_2GHZ_PRIVATE_WIFI);
			if (status) {
				status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
						RdkBWifiParameters.ENCRYPTION_MODE_2GHZ_PRIVATE_WIFI);
				if (!status) {
					errorMessage = "Default Encryption mode is not AES";
				}
			} else {
				errorMessage = "Default Security mode is not WPA2-Personal";
			}
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : The security mode is 'WPA2-Personal' and the Encryption method is \"AES\" as expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[1];
		status = false;
		errorMessage = "Network enabled status for 2.4GHz is not true";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION: Verify Wifi network enabled status for 2.4 GHz Wifi band");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION     : Execute Device.WiFi.SSID.10001.Status or .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   : The status should be  \"Up\"");
		LOGGER.info("**********************************************************************************");
		try {
			if (isSnmpNeeded) {
				String response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
						BroadBandSnmpMib.ECM_WIFI_2_4_SSID_STATUS.getOid());
				LOGGER.info("RESPONSE OF SNMPGET IS :" + response);

				if (CommonMethods.isNotNull(response)) {
					if (response.equals(BroadBandTestConstants.STRING_VALUE_ONE)) {
						status = true;
					} else {
						LOGGER.error("Value obtained from SNMP does not match the expected value.EXPECTED: "
								+ BroadBandTestConstants.STRING_VALUE_ONE + " ACTUAL :" + response);
					}
				} else
					status = false;
			} else {
				status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
						RdkBWifiParameters.SSID_STATUS_2GHZ_PRIVATE_WIFI);
			}
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : Network enabled status for 2.4GHz is True as expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[2];
		status = false;
		errorMessage = "Default channel selection mode  for 2.4GHz is not true";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION:Verify Wifi network channel selection mode for 2.4 GHz Wifi band.");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.Radio.10000.AutoChannelEnable");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The value should be 'true'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					RdkBWifiParameters.AUTOCHANNEL_ENABLE_STATUS_2GHZ);
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Default channel selection mode  for 2.4GHz is True as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[3];
		status = false;
		errorMessage = "Default channel bandwidth  for 2.4GHz is not matching with expected ";
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP " + stepNumber + " : DESCRIPTION:Verify Wifi network channel bandwidth for 2.4 GHz Wifi band.");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.Radio.10000.OperatingChannelBandwidth");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The value should be  recieved");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					(isFibreOrDSLDevice ? RdkBWifiParameters.OPERATING_CHANNEL_BANDWIDTH_2GHZ_FOR_FIBRE_DEVICES
							: RdkBWifiParameters.OPERATING_CHANNEL_BANDWIDTH_2GHZ));

		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Default channel bandwidth  for 2.4GHz is recieved as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[4];
		status = false;
		errorMessage = "Default network name enabled status for 2.4GHz is not true";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION:Verify Wifi Broadcast network Name enabled status for 2.4 GHz Wifi band");
		LOGGER.info(
				"STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The value should be 'true'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					RdkBWifiParameters.SSID_ADVERTISEMENT_ENABLE_2GHZ_PRIVATE_WIFI);
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Default network name enabled status for 2.4GHz is True as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[5];
		status = false;
		errorMessage = "Default guard interval  for 2.4GHz is not Auto";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION:Verify Guard interval for 2.4 GHz Wifi network");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.Radio.10000.GuardInterval");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The Guard interval should be 'Auto'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					RdkBWifiParameters.GUARD_INTERVAL_2GHZ);
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : Default guard interval  for 2.4GHz is Auto as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[6];
		status = false;
		errorMessage = "Default network active status for 2.4GHz is not true";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION:Verify WiFi network active status for 2.4 GHz ");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.SSID.10001.Enable");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The parameter value should be 'true'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					RdkBWifiParameters.SSID_ENABLE_STATUS_2GHZ_PRIVATE_WIFI);
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + " : ACTUAL : Default network active status for 2.4GHz is True as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[7];
		status = false;
		errorMessage = "Default operating standard for 2.4GHz is not g,n";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION:Verify the WiFi supported protocols for 2.4Ghz band  ");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.Radio.10000.OperatingStandards");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED   :The supported protocols should be 'g,n' or 'b,g,n' for DSL Device ");
		LOGGER.info("**********************************************************************************");
		try {
			String operatingStandards = BroadbandPropertyFileHandler.getRDKBWifiParametersBasedOnModel(device,
					BroadBandPropertyKeyConstants.PROP_KEY_DEVICE_OPERATING_STANDARDS_2GHZ);
			if (CommonMethods.isNotNull(operatingStandards)) {
				JSONObject jsonObj = new JSONObject(operatingStandards);
				status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap, jsonObj);
			} else {
				status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
						RdkBWifiParameters.OPERATING_STANDARDS_2GHZ);
			}
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}

		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : Default operating standard for 2.4GHz is g,n or b,g,n "
					+ "for DSL Device as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[8];
		status = false;
		errorMessage = "Default radio status for 2.4GHz is not true";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION:Verify WiFi radio status for 2.4 GHz ");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.Radio.10000.Enable");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The parameter value should be 'true'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					RdkBWifiParameters.RADIO_STATUS_2GHZ);
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : Default radio status for 2.4GHz is True as Expected");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		// Get All the webpa params for wifi settings 5GHZ
		wifiParameterMap = BroadBandRestoreWifiUtils.getAllWifiParametersForRadioUsingWebpa(device, tapEnv,
				WiFiFrequencyBand.WIFI_BAND_5_GHZ);

		stepNumber = stepNumbers[9];
		status = false;
		errorMessage = "The default security mode and encryption  are not WPA2-Personal and AES";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION:Verify Wifi network security mode for 5 GHz band ");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION     :Execute Device.WiFi.AccessPoint.10001.Security.ModeEnabled and Device.WiFi.AccessPoint.10001.Security.X_CISCO_COM_EncryptionMethod ");
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED   : The security mode should be 'WPA2-Personal' and the Encryption method should be \"AES\" ");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					RdkBWifiParameters.SECURITY_MODE_5GHZ_PRIVATE_WIFI);
			if (status) {
				status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
						RdkBWifiParameters.ENCRYPTION_MODE_5GHZ_PRIVATE_WIFI);
				if (!status) {
					errorMessage = "Default Encryption mode is not AES";
				}
			} else {
				errorMessage = "Default Security mode is not WPA2-Personal";
			}
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : The default security mode and encryption  are WPA2-Personal and AES as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[10];
		status = false;
		errorMessage = "Network enabled status for 5GHz is not true";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION: Verify Wifi network enabled status for 5 GHz Wifi band");
		LOGGER.info("STEP " + stepNumber
				+ " : ACTION     : Execute Device.WiFi.SSID.10101.Status or .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   : The status should be  \"Up\"");
		LOGGER.info("**********************************************************************************");

		try {
			if (isSnmpNeeded) {
				String response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
						BroadBandSnmpMib.ECM_WIFI_5_SSID_STATUS.getOid());
				LOGGER.info("RESPONSE OF SNMPGET IS :" + response);

				if (CommonMethods.isNotNull(response)) {
					if (response.equals(BroadBandTestConstants.STRING_VALUE_ONE)) {
						status = true;
					} else {
						LOGGER.error("Value obtained from SNMP does not match the expected value.EXPECTED: "
								+ BroadBandTestConstants.STRING_VALUE_ONE + " ACTUAL :" + response);
					}
				} else
					status = false;
			} else {
				status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
						RdkBWifiParameters.SSID_STATUS_5GHZ_PRIVATE_WIFI);
			}
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : Network enabled status for 5GHz is true as Expected");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[11];
		status = false;
		errorMessage = "Default channel selection mode  for 5GHz is not true";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION:Verify Wifi network channel selection mode for 5 GHz Wifi band.");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.Radio.10100.AutoChannelEnable");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The value should be 'true'");
		LOGGER.info("**********************************************************************************");
		if (!(DeviceModeHandler.isDSLDevice(device))) {
			try {
				if (!DeviceModeHandler.isRPIDevice(device)) {
					status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
							RdkBWifiParameters.AUTOCHANNEL_ENABLE_STATUS_5GHZ);
				} else {
					status = wifiParameterMap
							.get(BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ)
							.equalsIgnoreCase("false");
				}
			} catch (Exception e) {
				status = false;
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Default channel selection mode for 5GHz is true as Expected.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);
		} else {
			tapEnv.updateExecutionForAllStatus(device, testId, "s" + stepNumber, ExecutionStatus.NOT_APPLICABLE,
					"This test step is not applicable for DSL devices", false);
		}

		stepNumber = stepNumbers[12];
		status = false;
		errorMessage = "Default channel bandwidth  for 5GHz is not 80MHZ";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION:Verify Wifi network channel bandwidth for 5 GHz Wifi band.");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.Radio.10100.OperatingChannelBandwidth");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The value should be 80MHz");
		LOGGER.info("**********************************************************************************");

		try {
			String operatingStandards = BroadbandPropertyFileHandler.getRDKBWifiParametersBasedOnModel(device,
					BroadBandPropertyKeyConstants.PROP_KEY_DEVICE_OPERATING_CHANNEL_BANDWIDTH_5GHZ);
			if (CommonMethods.isNotNull(operatingStandards)) {
				JSONObject jsonObj = new JSONObject(operatingStandards);
				status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap, jsonObj);
			} else {
				status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
						RdkBWifiParameters.OPERATING_CHANNEL_BANDWIDTH_5GHZ);
			}
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : Default channel bandwidth  for 5GHz is 80MHZ as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[13];
		status = false;
		errorMessage = "Default network name enabled status for 5GHz is not true";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ " : DESCRIPTION:Verify Wifi Broadcast network Name enabled status for 5 GHz Wifi band");
		LOGGER.info(
				"STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The value should be 'true'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					RdkBWifiParameters.SSID_ADVERTISEMENT_ENABLE_5GHZ_PRIVATE_WIFI);
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Default network name enabled status for 5GHz is true as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[14];
		status = false;
		errorMessage = "Default guard interval  for 5GHz is not Auto";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION:Verify Guard interval for 5 GHz Wifi network");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.Radio.10100.GuardInterval");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The Guard interval should be 'Auto'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					RdkBWifiParameters.GUARD_INTERVAL_5GHZ);
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : Default guard interval for 5GHz is Auto as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[15];
		status = false;
		errorMessage = "Default network active status for 5GHz is not true";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION:Verify WiFi network active status for 5 GHz ");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.SSID.10101.Enable");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The parameter value should be 'true'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					RdkBWifiParameters.SSID_ENABLE_STATUS_5GHZ_PRIVATE_WIFI);
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + " : ACTUAL : Default network active status for 5GHz is True as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[16];
		status = false;
		errorMessage = "Default operating standard for 5GHz is not a,n,ac";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION:Verify the WiFi supported protocols for 5Ghz band  ");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.Radio.10100.OperatingStandards");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The default operating standard be 'a,n,ac' ");
		LOGGER.info("**********************************************************************************");
		try {

			String operatingStandards = BroadbandPropertyFileHandler.getRDKBWifiParametersBasedOnModel(device,
					BroadBandPropertyKeyConstants.PROP_KEY_DEVICE_OPERATING_STANDARDS_5GHZ);
			if (CommonMethods.isNotNull(operatingStandards)) {
				JSONObject jsonObj = new JSONObject(operatingStandards);
				status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap, jsonObj);
			} else {
				status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
						RdkBWifiParameters.OPERATING_STANDARDS_5GHZ);
			}
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info(
					"STEP " + stepNumber + " : ACTUAL : Default operating standard for 5GHz is a,n,ac as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);

		stepNumber = stepNumbers[17];
		status = false;
		errorMessage = "Default radio status for 5GHz is not true";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION:Verify WiFi radio status for 5 GHz ");
		LOGGER.info("STEP " + stepNumber + " : ACTION     :Execute Device.WiFi.Radio.10100.Enable");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED   :The parameter value should be 'true'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(wifiParameterMap,
					RdkBWifiParameters.RADIO_STATUS_5GHZ);
		} catch (Exception e) {
			status = false;
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : Default radio status for 5GHz is True as Expected.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, "s" + stepNumber, status, errorMessage, false);
	}

	/**
	 * Test to Verify WiFi settings like SSID ,Network Security mode,Channel
	 * Selection Mode,Channel bandwidth,Network name enabled status,Guard interval
	 * ,Network active status,Supported Protocols,Radio Status and Radio Channel can
	 * be restored to default settings using SNMP command for doing WIFI settings
	 * reset..
	 * 
	 * <li>Precondition:Set all WIFI parametrs to a non default value using
	 * webpa</li>
	 * <li>STEP 1:Restore WiFi Settings using snmp</li>
	 * <li>STEP 2:Verify Wifi network security mode for 2.4 GHz band</li>
	 * <li>STEP 3: Verify Wifi network enabled status for 2.4 GHz Wifi band</li>
	 * <li>STEP 4:Verify Wifi network channel selection mode for 2.4 GHz Wifi
	 * band.</li>
	 * <li>STEP 5:Verify Wifi network channel bandwidth for 2.4 GHz Wifi band</li>
	 * <li>STEP 6: Verify Wifi Broadcast network Name enabled status for 2.4 GHz
	 * Wifi band</li>
	 * <li>STEP 7: Verify Guard interval for 2.4 GHz Wifi network</li>;
	 * <li>STEP 8: Verify WiFi network active status for 2.4 GHz in Connection
	 * Status page</li>
	 * <li>STEP 9: Verify the WiFi supported protocols for 2.4Ghz band</li>
	 * <li>STEP 10: Verify the Radio status for 2.4GHZ band using WebPA</li>
	 * <li>STEP 11: Verify Wifi network security mode for 5 GHz band</li>
	 * <li>STEP 12: Verify Wifi network enabled status for 5 GHz Wifi band</li>
	 * <li>STEP 13: Verify Wifi network channel selection mode for 5 GHz Wifi
	 * band</li>
	 * <li>STEP 14:Verify Wifi network channel bandwidth for 5GHz Wifi band</li>
	 * <li>STEP 15: Verify Wifi Broadcast network Name enabled status for 5 GHz Wifi
	 * band</li>
	 * <li>STEP 16: Verify Guard interval for 5 GHz Wifi network</li>
	 * <li>STEP 17: Verify WiFi network active status for 5GHz in Connection Status
	 * page</li>
	 * <li>STEP 18: Verify the WiFi supported protocols for 5 Ghz band</li>
	 * <li>STEP 19: Verify the Radio status for 5GHZ band using WebPA</li>
	 * 
	 * @param device {@link Dut}
	 * @author anandam.s
	 * @refactor yamini.s
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-RESTORE-1002")
	public void testVerifyWifiDefaultSettingsAfterWiFiResetUsingSnmp(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-RESTORE-102";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;

		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: " + testId);
			LOGGER.info(
					"TEST DESCRIPTION: Verify WiFi settings can be restored to default settings using snmp command for setting reset");

			LOGGER.info("Precondition:Set the below parametrs to a non  default value using webpa");
			LOGGER.info("1. SSID name");
			LOGGER.info("2. Network Security mode ");
			LOGGER.info("3. Network Enabled status");
			LOGGER.info("4. Channel Selection Mode ");
			LOGGER.info("5. Channel bandwidth");
			LOGGER.info("6. Netwok name enabled sttaus ");
			LOGGER.info("7. Guard interval");
			LOGGER.info("8. Network active status");
			LOGGER.info("9.Supported Protocols");
			LOGGER.info("10.Radio Status");
			LOGGER.info("11.Radio Channels");
			LOGGER.info("STEP 1:Restore WiFi Settings using snmp ");
			LOGGER.info("STEP 2:Verify Wifi network security mode for 2.4 GHz band ");
			LOGGER.info("STEP 3: Verify Wifi network enabled status for 2.4 GHz Wifi band");
			LOGGER.info("STEP 4:Verify Wifi network channel selection mode for 2.4 GHz Wifi band.");
			LOGGER.info("STEP 5:Verify Wifi network channel bandwidth for 2.4 GHz Wifi band");
			LOGGER.info("STEP 6: Verify Wifi Broadcast network Name enabled status for 2.4 GHz Wifi band ");
			LOGGER.info("STEP 7: Verify Guard interval for 2.4 GHz Wifi network ");
			LOGGER.info("STEP 8: Verify WiFi network active status for 2.4 GHz in Connection Status page");
			LOGGER.info("STEP 9: Verify the WiFi supported protocols for 2.4Ghz band ");
			LOGGER.info("STEP 10: Verify the Radio status for 2.4GHZ band using WebPA");
			LOGGER.info("STEP 11: Verify Wifi network security mode for 5 GHz band");
			LOGGER.info("STEP 12: Verify Wifi network enabled status for 5 GHz Wifi band");
			LOGGER.info("STEP 13: Verify Wifi network channel selection mode for 5 GHz Wifi band");
			LOGGER.info("STEP 14:Verify Wifi network channel bandwidth for 5GHz Wifi band");
			LOGGER.info("STEP 15: Verify Wifi Broadcast network Name enabled status for 5 GHz Wifi band");
			LOGGER.info("STEP 16: Verify Guard interval for 5 GHz Wifi network");
			LOGGER.info("STEP 17: Verify WiFi network active status for 5GHz in Connection Status page");
			LOGGER.info("STEP 18: Verify the WiFi supported protocols for 5 Ghz band");
			LOGGER.info("STEP 19: Verify the Radio status for 5GHZ band using WebPA");
			LOGGER.info("#######################################################################################");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("Precondition:Set the below parametrs to a non  default value using webpa");
			LOGGER.info("1. SSID name");
			LOGGER.info("2. Network Security mode ");
			LOGGER.info("3. Network Enabled status");
			LOGGER.info("4. Channel Selection Mode ");
			LOGGER.info("5. Channel bandwidth");
			LOGGER.info("6. Netwok name enabled sttaus ");
			LOGGER.info("7. Guard interval");
			LOGGER.info("8. Network active status");
			LOGGER.info("9.Supported Protocols");
			LOGGER.info("10.Radio Status");
			LOGGER.info("11.Radio Channels");
			LOGGER.info("EXPECTED: All the test parameters must be set to non default values");
			LOGGER.info("#####################################################################################");
			try {
				status = BroadBandRestoreWifiUtils.setNonDefaultValuesForAllWifiParameters(device, tapEnv);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION : ACTUAL : PRE CONDITION EXECUTED SUCCESSFULLY");
			} else {
				LOGGER.error("PRE-CONDITION : ACTUAL : " + errorMessage);
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

			LOGGER.info("Waiting for 2 minutes for changes to get reflected");
			tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);

			testStepNumber = "s1";
			status = false;
			errorMessage = "Restore WIFI settings using snmp failed";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Restore WiFi Settings using SNMP ");
			LOGGER.info(
					"STEP 1: ACTION      : Set the SNMP snmpset -v2c -c udp6:[ecmip] .1.3.6.1.4.1.17270.50.2.1.1.1002.0 i 3 ");
			LOGGER.info("STEP 1: EXPECTED    : SNMP set must be success");
			LOGGER.info("**********************************************************************************");
			if (!DeviceModeHandler.isRPIDevice(device)) {
				status = BroadBandRestoreWifiUtils.restoreDefaultWifiSettings(device, tapEnv, WIFI_RESTORE_METHOD.SNMP);
			} else {
				status = BroadBandCommonUtils.performFactoryResetSnmp(tapEnv, device);
				;
			}

			if (status) {
				LOGGER.info("STEP 1:ACTUAL : Wi-Fi Settings restored successfully");
			} else {
				LOGGER.error("STEP 1:ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			LOGGER.info("Waiting for 6 minutes  for changes to get affected");
			tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS * 2);

			/** Steps 2 to 19 */
			verifyAllWifiParameters(device, testId, new String[] { "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9",
					"s10", "s11", "s12", "s13", "s14", "s15", "s16", "s17", "s18", "s19" }, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception occured during execution !!!!" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-RESTORE-1002");
	}

}
