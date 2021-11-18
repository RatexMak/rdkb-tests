/**
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

/**
 * Class for Validating the Configuration of Transmission Rates for both 2.4GHz Radio and 5GHz radio on BroadBand
 * Devices.
 */

import org.testng.annotations.Test;
import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;

public class BroadBandWifiTxRateConfigurationTest extends AutomaticsTestBase {

	/**
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>STEP 1: check the country code</li>
	 * <li>STEP 2: Set channel to 153</li>
	 * <li>STEP 3: Apply radio settings</li>
	 * <li>STEP 4: Check the Channel</li>
	 * <li>STEP 5: Check the power limit in 153 channel</li>
	 * <li>STEP 6: To check power limit at mid range of UNII-1 channels.Set UNII-1
	 * channel to 40</li>
	 * <li>STEP 7:Apply radio settings</li>
	 * <li>STEP 8: Check the Channel</li>
	 * <li>STEP 9: Check the power limit in 40 channel</li>
	 * <li>STEP 10: To check power limit at low range of UNII-1 channels.Set UNII-1
	 * channel to 36</li>
	 * <li>STEP 11:Apply radio settings</li>
	 * <li>STEP 12: Check the Channel</li>
	 * <li>STEP 13: Check the power limit in 36 channel</li>
	 * <li>STEP 14: To check power limit at high range of UNII-1 channels.Set UNII-1
	 * channel to 48</li>
	 * <li>STEP 15:Apply radio settings</li>
	 * <li>STEP 16: Check the Channel</li>
	 * <li>STEP 17: Check the power limit in 48 channel</li>
	 * </ol>
	 * 
	 * @author Deepa Bada
	 * @refactor Athira
	 * 
	 * @param device {@link Dut}
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
		    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-TX-RATE-CONFIG-5064", testDecription = "Verify default SSID using WebPA ")
	public void testHighPowerSupportOn5GBandChannels(Dut device) {
	String testCaseId = "TC-RDKB-WIFI-TX-RATE-CONFIG-564";
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	String channelValue = null;
	String meshInitialStatus = null;
	String response = null;
	int stepCounter = BroadBandTestConstants.CONSTANT_0;
	try {
	    LOGGER.info("************************* STARTING PRE-CONFIGURATIONS *************************");
	    meshInitialStatus= BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
	    BroadBandPreConditionUtils.executePreConditionToDisableMeshUsingSystemCommands(device, tapEnv,meshInitialStatus,
			    BroadBandTestConstants.CONSTANT_1);;
	    LOGGER.info("************************* COMPLETED PRE-CONFIGURATIONS *************************");

	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-TX-RATE-CONFIG-564");
	    LOGGER.info("TEST DESCRIPTION: To verfiy high power support in 5GHZ band channel");    
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("STEP 1: check the country code");
	    LOGGER.info("STEP 2: Set channel to 153");
	    LOGGER.info("STEP 3: Apply radio settings");
	    LOGGER.info("STEP 4: Check the Channel");
	    LOGGER.info("STEP 5: Check the power limit in 153 channel");
	    LOGGER.info("STEP 6: To check power limit at mid range of UNII-1 channels.Set UNII-1 channel to 40");
	    LOGGER.info("STEP 7:Apply radio settings");
	    LOGGER.info("STEP 8: Check the Channel");
	    LOGGER.info("STEP 9: Check the power limit in 40 channel");
	    LOGGER.info("STEP 10: To check power limit at low range of UNII-1 channels.Set UNII-1 channel to 36");
	    LOGGER.info("STEP 11:Apply radio settings");
	    LOGGER.info("STEP 12: Check the Channel");
	    LOGGER.info("STEP 13: Check the power limit in 36 channel");
	    LOGGER.info("STEP 14: To check power limit at high range of UNII-1 channels.Set UNII-1 channel to 48");
	    LOGGER.info("STEP 15:Apply radio settings");
	    LOGGER.info("STEP 16: Check the Channel");
	    LOGGER.info("STEP 17: Check the power limit in 48 channel");
	    
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Set channel to 153 ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : check the current channel and Set channel to 153 ");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.Channel uint 153 ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :set is successful");
	    LOGGER.info("**********************************************************************************");
	    channelValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
	    if (CommonMethods.isNotNull(channelValue)) {
		if (!channelValue.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_153)) {
		    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
			    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.CONSTANT_153);
		} else
		    status = true;
	    }
	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel set to 153 Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;
	    errorMessage = "Unable to Apply radio settings  ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Apply radio settings ");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.X_CISCO_COM_ApplySetting bool true");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Settings applied");
	    LOGGER.info("**********************************************************************************");

	    WebPaParameter webPaParameter = new WebPaParameter();
	    webPaParameter.setName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING);
	    webPaParameter.setValue(BroadBandTestConstants.TRUE);
	    webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
	    
	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Radio settings applied Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;
	    
	    errorMessage = "Unable to Check the Channel  ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the Channel  ");
	    LOGGER.info(
		    "STEP " + stepCounter + ": ACTION :Execute command :dmcli eRT getv Device.WiFi.Radio.2.Channel ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Channel is 153");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
	    status = CommonMethods.isNotNull(response) && response.contains(BroadBandTestConstants.CONSTANT_153);

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel verified Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Check the power limit in 153 channel ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the power limit in 153 channel ");
	    LOGGER.info("STEP " + stepCounter + ": ACTION :Execute command :iwlist ath1 txpower ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :power range is 25 -30dbm");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_TXPOWER);
	    response = CommonMethods.patternFinder(response, BroadBandCommandConstants.PATTERN_TO_FIND_POWERLIMIT);
	    if (CommonMethods.isNotNull(response)) {
		int value = Integer.parseInt(response);
		if (value >= BroadBandTestConstants.CONSTANT_25 && value <= BroadBandTestConstants.CONSTANT_30)
		    status = true;

	    }
	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL :  power limit verified Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;
	    
	    errorMessage = "Unable to check power limit at mid range of UNII-1 channels.Set UNII-1 channel to 40 ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter
		    + ": DESCRIPTION : To check power limit at mid range of UNII-1 channels.Set UNII-1 channel to 40");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.Channel uint 40 ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :set is successful");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
			    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_FORTY);

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel set to 40 Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Apply radio settings  ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Apply radio settings ");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.X_CISCO_COM_ApplySetting bool true");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Settings applied");
	    LOGGER.info("**********************************************************************************");

	    webPaParameter.setName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING);
	    webPaParameter.setValue(BroadBandTestConstants.TRUE);
	    webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
	    status = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
	    
	    if (status) {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : Radio settings applied Successfully  ");
		    } else {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    }
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
		    stepCounter += BroadBandTestConstants.CONSTANT_1;
		    stepNum = "s" + stepCounter;

		    errorMessage = "Unable to Check the Channel  ";
		    LOGGER.info("**********************************************************************************");
		    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the Channel  ");
		    LOGGER.info(
			    "STEP " + stepCounter + ": ACTION :Execute command :dmcli eRT getv Device.WiFi.Radio.2.Channel ");
		    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Channel is 40");
		    LOGGER.info("**********************************************************************************");

		    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
		    status = CommonMethods.isNotNull(response) && response.contains(BroadBandTestConstants.STRING_VALUE_FORTY);

		    if (status) {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : channel verified Successfully  ");
		    } else {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    }
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		    stepCounter += BroadBandTestConstants.CONSTANT_1;
		    stepNum = "s" + stepCounter;
		    
		    errorMessage = "Unable to Check the power limit in 40 channel ";
		    LOGGER.info("**********************************************************************************");
		    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the power limit in 40 channel ");
		    LOGGER.info("STEP " + stepCounter + ": ACTION :Execute command :iwlist ath1 txpower ");
		    LOGGER.info("STEP " + stepCounter + ": EXPECTED :power range is 25 -30dbm");
		    LOGGER.info("**********************************************************************************");

		    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_TXPOWER);
		    response = CommonMethods.patternFinder(response, BroadBandCommandConstants.PATTERN_TO_FIND_POWERLIMIT);
		    if (CommonMethods.isNotNull(response)) {
			int value = Integer.parseInt(response);
			if (value >= BroadBandTestConstants.CONSTANT_25 && value <= BroadBandTestConstants.CONSTANT_30)
			    status = true;

		    }

		    if (status) {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL :  power limit verified Successfully");
		    } else {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    }
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		    stepCounter += BroadBandTestConstants.CONSTANT_1;
		    stepNum = "s" + stepCounter;

		    errorMessage = "Unable to check power limit at low range of UNII-1 channels.Set UNII-1 channel to 36 ";
		    LOGGER.info("**********************************************************************************");
		    LOGGER.info("STEP " + stepCounter
			    + ": DESCRIPTION : To check power limit at low range of UNII-1 channels.Set UNII-1 channel to 36");
		    LOGGER.info("STEP " + stepCounter
			    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.Channel uint 36 ");
		    LOGGER.info("STEP " + stepCounter + ": EXPECTED :set is successful");
		    LOGGER.info("**********************************************************************************");

		    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
			    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.RADIO_CHANNEL_36);

		    if (status) {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel set to 36 Successfully  ");
		    } else {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    }
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
		    stepCounter += BroadBandTestConstants.CONSTANT_1;
		    stepNum = "s" + stepCounter;

		    errorMessage = "Unable to Apply radio settings  ";
		    LOGGER.info("**********************************************************************************");
		    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Apply radio settings ");
		    LOGGER.info("STEP " + stepCounter
			    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.X_CISCO_COM_ApplySetting bool true");
		    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Settings applied");
		    LOGGER.info("**********************************************************************************");

		    webPaParameter.setName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING);
		    webPaParameter.setValue(BroadBandTestConstants.TRUE);
		    webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
		    status = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);

		    if (status) {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : Radio settings applied Successfully  ");
		    } else {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    }
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
		    stepCounter += BroadBandTestConstants.CONSTANT_1;
		    stepNum = "s" + stepCounter;

		    errorMessage = "Unable to Check the Channel  ";
		    LOGGER.info("**********************************************************************************");
		    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the Channel  ");
		    LOGGER.info(
			    "STEP " + stepCounter + ": ACTION :Execute command :dmcli eRT getv Device.WiFi.Radio.2.Channel ");
		    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Channel is 36");
		    LOGGER.info("**********************************************************************************");

		    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
		    status = CommonMethods.isNotNull(response) && response.contains(BroadBandTestConstants.RADIO_CHANNEL_36);

		    if (status) {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel verified Successfully  ");
		    } else {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    }
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		    stepCounter += BroadBandTestConstants.CONSTANT_1;
		    stepNum = "s" + stepCounter;

		    errorMessage = "Unable to Check the power limit in 36 channel ";
		    LOGGER.info("**********************************************************************************");
		    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the power limit in 36 channel ");
		    LOGGER.info("STEP " + stepCounter + ": ACTION :Execute command :iwlist ath1 txpower ");
		    LOGGER.info("STEP " + stepCounter + ": EXPECTED :power range is 25 -30dbm");
		    LOGGER.info("**********************************************************************************");

		    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_TXPOWER);
		    response = CommonMethods.patternFinder(response, BroadBandCommandConstants.PATTERN_TO_FIND_POWERLIMIT);
		    if (CommonMethods.isNotNull(response)) {
			int value = Integer.parseInt(response);
			if (value >= BroadBandTestConstants.CONSTANT_25 && value <= BroadBandTestConstants.CONSTANT_30)
			    status = true;

		    }

		    if (status) {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL :  power limit verified Successfully  ");
		    } else {
			LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    }
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		    stepCounter += BroadBandTestConstants.CONSTANT_1;
		    stepNum = "s" + stepCounter;

		    errorMessage = "Unable to check power limit at high range of UNII-1 channels.Set UNII-1 channel to 48 ";
		    LOGGER.info("**********************************************************************************");
		    LOGGER.info("STEP " + stepCounter
			    + ": DESCRIPTION : To check power limit at high range of UNII-1 channels.Set UNII-1 channel to 48");
		    LOGGER.info("STEP " + stepCounter
			    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.Channel uint 36 ");
		    LOGGER.info("STEP " + stepCounter + ": EXPECTED :set is successful");
		    LOGGER.info("**********************************************************************************");

		    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
			    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.RADIO_CHANNEL_48);
		    if (status) {
				LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel set to  48 Successfully  ");
			    } else {
				LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
			    }
			    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			    stepCounter += BroadBandTestConstants.CONSTANT_1;
			    stepNum = "s" + stepCounter;

			    errorMessage = "Unable to Apply radio settings  ";
			    LOGGER.info("**********************************************************************************");
			    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Apply radio settings ");
			    LOGGER.info("STEP " + stepCounter
				    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.X_CISCO_COM_ApplySetting bool true");
			    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Settings applied");
			    LOGGER.info("**********************************************************************************");

			    webPaParameter.setName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING);
			    webPaParameter.setValue("true");
			    webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
			    status = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
			    if (status) {
				LOGGER.info("STEP " + stepCounter + ": ACTUAL : Radio settings applied Successfully  ");
			    } else {
				LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
			    }
			    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			    stepCounter += BroadBandTestConstants.CONSTANT_1;
			    stepNum = "s" + stepCounter;

			    errorMessage = "Unable to Check the Channel  ";
			    LOGGER.info("**********************************************************************************");
			    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the Channel  ");
			    LOGGER.info(
				    "STEP " + stepCounter + ": ACTION :Execute command :dmcli eRT getv Device.WiFi.Radio.2.Channel ");
			    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Channel is 48");
			    LOGGER.info("**********************************************************************************");

			    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
			    status = CommonMethods.isNotNull(response) && response.contains(BroadBandTestConstants.RADIO_CHANNEL_48);
			    if (status) {
					LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel verified Successfully  ");
				    } else {
					LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
				    }
				    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
				    stepCounter += BroadBandTestConstants.CONSTANT_1;
				    stepNum = "s" + stepCounter;

				    errorMessage = "Unable to Check the power limit in 36 channel ";
				    LOGGER.info("**********************************************************************************");
				    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the power limit in 36 channel ");
				    LOGGER.info("STEP " + stepCounter + ": ACTION :Execute command :iwlist ath1 txpower ");
				    LOGGER.info("STEP " + stepCounter + ": EXPECTED :power range is 25 -30dbm");
				    LOGGER.info("**********************************************************************************");

				    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_TXPOWER);
				    response = CommonMethods.patternFinder(response, BroadBandCommandConstants.PATTERN_TO_FIND_POWERLIMIT);
				    if (CommonMethods.isNotNull(response)) {
					int value = Integer.parseInt(response);
					if (value >= BroadBandTestConstants.CONSTANT_25 && value <= BroadBandTestConstants.CONSTANT_30)
					    status = true;

				    }

				    if (status) {
					LOGGER.info("STEP " + stepCounter + ": ACTUAL : power limit verified Successfully  ");
				    } else {
					LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
				    }
				    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	}
	catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	}
}
